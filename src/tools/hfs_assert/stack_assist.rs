use std::collections::HashMap;
use std::rc::Rc;

use z3::ast::{Array, Bool, Int};
use z3::{SatResult, Solver, Sort};

use crate::hfs::error::{CompileError, DiagnosticInfo};
use crate::hfs::{Expression, Span, StackBlockExpression, StackEnvironment, StackModifier, VALID_STACK_KEYWORDS};
use crate::stack_assist_error;
use crate::tools::hfs_assert::stack_assist_errors::{StackAssistError, StackAssistErrorKind};

pub struct EncodingContext {
    pub solver: Solver,
    pub state: Array,
    pub sp: i64,
    pub next_id: i64,
    pub names: HashMap<String, i64>,
    pub lits: Vec<Bool>, // assumption literals, used to track which step is part of the MUS
}

impl EncodingContext {
    pub fn new() -> Self {
        Self {
            solver: Solver::new(),
            state: Array::new_const("s0", &Sort::int(), &Sort::int()),
            sp: 0,
            next_id: 0,
            names: HashMap::new(),
            lits: vec![],
        }
    }

    pub fn read_stack_at(&self, d: i64) -> Int {
        self.state.select(&Int::from_i64(self.sp - d)).as_int().unwrap()
    }

    pub fn commit(&mut self, next: Array) {
        let k = self.lits.len();
        let p = Bool::new_const(format!("step_{k}"));
        let new = Array::new_const(format!("s{}", k + 1), &Sort::int(), &Sort::int());
        self.solver.assert(&p.implies(&new.eq(&next)));
        self.state = new;
        self.lits.push(p);
    }
}

pub struct StackAssistant {
    pub diagnostic_info: Rc<DiagnosticInfo>,
}

impl StackAssistant {
    pub fn new(diagnostic_info: Rc<DiagnosticInfo>) -> Self {
        Self { diagnostic_info }
    }

    pub fn normalize_environment(&self, environment: StackEnvironment) -> Result<StackEnvironment, Box<dyn CompileError>> {
        match (
            environment.modifiers.iter().find(|m| matches!(m, StackModifier::Pre(_))),
            environment.modifiers.iter().find(|m| matches!(m, StackModifier::Post(_))),
        ) {
            (None, None) => unreachable!(
                "[internal error] somehow found no pre or post conditions inside an environment that entered the stack assistant"
            ),
            (None, Some(m)) => Ok(StackEnvironment {
                modifiers: vec![StackModifier::Pre(vec![]), m.clone()],
                resolved_body: environment.resolved_body,
                span: environment.span,
            }),
            (Some(_), None) => return stack_assist_error!(StackAssistErrorKind::NoPostconditionFound, &self, environment.span),
            (Some(_), Some(_)) => Ok(environment),
        }
    }

    pub fn find_error_and_correction(&self, environment: StackEnvironment) -> Result<(), Box<dyn CompileError>> {
        let normalized_environment = self.normalize_environment(environment.clone())?;

        let (solver, lits) = self.generate_variables_and_constraints(&normalized_environment);
        dbg!(solver.get_assertions());
        dbg!(solver.check_assumptions(&lits));

        if solver.check_assumptions(&lits) == SatResult::Unsat {
            let core = solver.get_unsat_core();
            let mus = self.minimize_core(core);
            let mus_span = self.get_mus_span(&mus);
            let mcs = self.find_mcs(&mus);
            let correction = self.find_correction(mcs, &normalized_environment);
            return stack_assist_error!(StackAssistErrorKind::AssertionFailed(mus_span, correction), &self, environment.span);
        }
        Ok(())
    }

    fn generate_variables_and_constraints(&self, environment: &StackEnvironment) -> (Solver, Vec<Bool>) {
        let mut ctx = EncodingContext::new();

        if let Some(StackModifier::Pre(idents)) = environment.modifiers.iter().find(|m| matches!(m, StackModifier::Pre(_))) {
            self.encode_precondition(idents, &mut ctx);
        }

        for expr in environment.resolved_body.iter() {
            match expr {
                StackBlockExpression::Expr(expression) => match expression {
                    Expression::Operation(operation) => todo!(),
                    Expression::Identifier(identifier) => todo!(),
                    Expression::Literal(literal) => todo!(),
                    Expression::Tuple { expressions } => todo!(),
                    Expression::ArrayLiteral { elements } => todo!(),
                    Expression::Parameter { index, type_id } => todo!(),
                    Expression::ReturnValue(type_id) => todo!(),
                },
                StackBlockExpression::StackKeyword(keyword) => match keyword {
                    keyword if !VALID_STACK_KEYWORDS.contains(&keyword.as_str()) => {
                        unreachable!("[internal error] somehow found an invalid stack keyword in the stack assistant")
                    },
                    _ => self.encode_stack_keyword(keyword, &mut ctx),
                },
            }
        }

        if let Some(StackModifier::Post(idents)) = environment.modifiers.iter().find(|m| matches!(m, StackModifier::Post(_))) {
            self.encode_postcondition(idents, &mut ctx);
        }

        (ctx.solver, ctx.lits)
    }

    fn minimize_core(&self, core: Vec<z3::ast::Bool>) -> Vec<z3::ast::Bool> {
        vec![]
    }

    fn find_mcs(&self, mus: &Vec<z3::ast::Bool>) -> Vec<z3::ast::Bool> {
        vec![]
    }

    fn find_correction(&self, mcs: Vec<z3::ast::Bool>, environment: &StackEnvironment) -> String {
        "".to_string()
    }

    fn get_mus_span(&self, mus: &Vec<z3::ast::Bool>) -> Span {
        Span::default()
    }

    fn encode_precondition(&self, idents: &[String], ctx: &mut EncodingContext) {
        // the precondition never fails so we never add any constraints
        // we just build the initial state and store it in the context, so it can be asserted later
        for ident in idents {
            ctx.names.insert(ident.clone(), ctx.next_id);
            ctx.state = ctx.state.store(&Int::from_i64(ctx.sp), &Int::from_i64(ctx.next_id));
            ctx.sp += 1;
            ctx.next_id += 1;
        }
    }

    fn encode_postcondition(&self, post: &[String], ctx: &mut EncodingContext) {
        let len = post.len() as i64;

        for (j, name) in post.iter().enumerate() {
            // we get the value in the current stack that corresponds to that postcondition identifier
            let slot_j = ctx.read_stack_at(len - j as i64);
            let q = Bool::new_const(format!("post_{j}"));

            // if a name from the pre pins this slot to that pre value, we assert equality between them
            if let Some(&id) = ctx.names.get(name) {
                ctx.solver.assert(&q.implies(&slot_j.eq(&Int::from_i64(id))));
            }

            // same name => same value, different name => different value
            for i in 0..j {
                let slot_i = ctx.read_stack_at(len - i as i64);
                let same = slot_j.eq(&slot_i);
                let constraint = if post[i] == *name { same } else { same.not() };
                ctx.solver.assert(&q.implies(&constraint));
            }

            ctx.lits.push(q);
        }
    }

    fn encode_stack_keyword(&self, keyword: &str, ctx: &mut EncodingContext) {
        // stack keywords simply simulate the operation on a new z3 array, then commit that array as a new step
        match keyword {
            "@pop" => {
                let next = ctx.state.clone();
                ctx.commit(next);
                ctx.sp -= 1;
            },
            "@dup" => {
                let next = ctx.state.store(&Int::from_i64(ctx.sp), &ctx.read_stack_at(1));
                ctx.commit(next);
                ctx.sp += 1;
            },
            "@swap" => {
                let (a, b) = (ctx.read_stack_at(2), ctx.read_stack_at(1));
                let next = ctx.state.store(&Int::from_i64(ctx.sp - 2), &b).store(&Int::from_i64(ctx.sp - 1), &a);
                ctx.commit(next);
            },
            "@pop_all" => {
                let next = ctx.state.clone();
                ctx.commit(next);
                ctx.sp = 0;
            },
            "@over" => {
                let a = ctx.read_stack_at(2);
                let next = ctx.state.store(&Int::from_i64(ctx.sp), &a);
                ctx.sp += 1;
                ctx.commit(next);
            },
            "@rot" => {
                let (a, b, c) = (ctx.read_stack_at(3), ctx.read_stack_at(2), ctx.read_stack_at(1));
                let next = ctx
                    .state
                    .store(&Int::from_i64(ctx.sp - 3), &b)
                    .store(&Int::from_i64(ctx.sp - 2), &c)
                    .store(&Int::from_i64(ctx.sp - 1), &a);
                ctx.commit(next);
            },
            "@rrot" => {
                let (a, b, c) = (ctx.read_stack_at(3), ctx.read_stack_at(2), ctx.read_stack_at(1));
                let next = ctx
                    .state
                    .store(&Int::from_i64(ctx.sp - 3), &c)
                    .store(&Int::from_i64(ctx.sp - 2), &a)
                    .store(&Int::from_i64(ctx.sp - 1), &b);
                ctx.commit(next);
            },
            "@nip" => {
                let b = ctx.read_stack_at(1);
                let next = ctx.state.store(&Int::from_i64(ctx.sp - 2), &b);
                ctx.sp -= 1;
                ctx.commit(next);
            },
            "@tuck" => {
                let (a, b) = (ctx.read_stack_at(2), ctx.read_stack_at(1));
                let next = ctx
                    .state
                    .store(&Int::from_i64(ctx.sp - 2), &b)
                    .store(&Int::from_i64(ctx.sp - 1), &a)
                    .store(&Int::from_i64(ctx.sp), &b);
                ctx.sp += 1;
                ctx.commit(next);
            },
            "@rev" => {
                let mut next = ctx.state.clone();

                for i in 0..ctx.sp / 2 {
                    let left = Int::from_i64(i);
                    let right = Int::from_i64(ctx.sp - 1 - i);

                    let a = ctx.state.select(&left).as_int().unwrap();
                    let b = ctx.state.select(&right).as_int().unwrap();

                    next = next.store(&left, &b).store(&right, &a);
                }

                ctx.commit(next);
            },
            "@depth" => {
                let next = ctx.state.store(&Int::from_i64(ctx.sp), &Int::from_i64(ctx.next_id));

                ctx.sp += 1;
                ctx.next_id += 1;
                ctx.commit(next);
            },
            _ => panic!("[internal error] found invalid stack keyword when generating encoding"),
        }
    }
}
