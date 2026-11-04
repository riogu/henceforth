use std::collections::HashMap;
use std::rc::Rc;

use z3::ast::{Array, Bool, Int};
use z3::{SatResult, Solver, Sort};

use crate::hfs::error::{CompileError, DiagnosticInfo};
use crate::hfs::{Expression, Span, StackBlockExpression, StackEnvironment, StackModifier, VALID_STACK_KEYWORDS};
use crate::stack_assist_error;
use crate::tools::hfs_assert::marco_enumerator::MarcoEnumerator;
use crate::tools::hfs_assert::stack_assist_errors::{StackAssistError, StackAssistErrorKind};

#[derive(Clone)]
pub struct TrackedConstraint {
    pub literal: Bool,
    pub span: Span,
}

pub struct EncodingContext {
    pub solver: Solver,
    pub state: Array,
    pub sp: i64,
    pub next_id: i64,
    pub names: HashMap<String, i64>,
    pub constraints: Vec<TrackedConstraint>, // assumption literals, used to track which step is part of the MUS
    pub next_step: usize,
    pub next_post: usize,
}

impl EncodingContext {
    pub fn new() -> Self {
        Self {
            solver: Solver::new(),
            state: Array::new_const("s0", &Sort::int(), &Sort::int()),
            sp: 0,
            next_id: 0,
            names: HashMap::new(),
            constraints: vec![],
            next_step: 0,
            next_post: 0,
        }
    }

    pub fn read_stack_at(&self, d: i64) -> Int {
        self.state.select(&Int::from_i64(self.sp - d)).as_int().unwrap()
    }

    pub fn commit(&mut self, next: Array, span: &Span) {
        let k = self.next_step;
        let p = Bool::new_const(format!("step_{k}"));
        let new = Array::new_const(format!("s{}", k + 1), &Sort::int(), &Sort::int());
        self.solver.assert(&p.implies(&new.eq(&next)));
        self.state = new;
        self.constraints.push(TrackedConstraint { literal: p, span: *span });
        self.next_step += 1;
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
            environment.modifiers.iter().find(|m| matches!(m, StackModifier::Pre(_, _))),
            environment.modifiers.iter().find(|m| matches!(m, StackModifier::Post(_, _))),
        ) {
            (None, None) => unreachable!(
                "[internal error] somehow found no pre or post conditions inside an environment that entered the stack assistant"
            ),
            (None, Some(m)) => Ok(StackEnvironment {
                modifiers: vec![StackModifier::Pre(vec![], Span::default()), m.clone()],
                resolved_body: environment.resolved_body,
                span: environment.span,
            }),
            (Some(_), None) => return stack_assist_error!(StackAssistErrorKind::NoPostconditionFound, &self, vec![environment.span]),
            (Some(_), Some(_)) => Ok(environment),
        }
    }

    pub fn find_error_and_correction(&self, environment: StackEnvironment) -> Result<(), Box<dyn CompileError>> {
        let normalized_environment = self.normalize_environment(environment.clone())?;

        let (solver, constraints) = self.generate_variables_and_constraints(&normalized_environment);

        dbg!(&solver.get_assertions());
        let marco = MarcoEnumerator::new(solver.clone(), constraints.clone());
        let result = marco.enumerate()?;
        dbg!(&result);

        if result.muses.is_empty() {
            let assumptions: Vec<Bool> = constraints.iter().map(|c| c.literal.clone()).collect();

            assert_eq!(solver.check_assumptions(&assumptions), SatResult::Sat,);
        }

        if !result.muses.is_empty() {
            match self.get_error_locations(&result.muses, &constraints).first() {
                Some(loc) => return stack_assist_error!(StackAssistErrorKind::AssertionFailed, &self, loc.clone()),
                None => panic!("[internal error] found muses but found no error locations"),
            }
        }

        Ok(())
    }

    fn generate_variables_and_constraints(&self, environment: &StackEnvironment) -> (Solver, Vec<TrackedConstraint>) {
        let mut ctx = EncodingContext::new();

        if let Some(StackModifier::Pre(idents, _)) = environment.modifiers.iter().find(|m| matches!(m, StackModifier::Pre(_, _)))
        {
            self.encode_precondition(idents, &mut ctx);
        }

        for expr in environment.resolved_body.iter() {
            match expr {
                StackBlockExpression::Expr(expression, span) => match expression {
                    Expression::Operation(operation) => todo!(),
                    Expression::Identifier(identifier) => todo!(),
                    Expression::Literal(literal) => todo!(),
                    Expression::Tuple { expressions } => todo!(),
                    Expression::ArrayLiteral { elements } => todo!(),
                    Expression::Parameter { .. } | Expression::ReturnValue(_) => {
                        panic!("[internal error] found function related expressions inside a stack environment")
                    },
                },
                StackBlockExpression::StackKeyword(keyword, span) => match keyword {
                    keyword if !VALID_STACK_KEYWORDS.contains(&keyword.as_str()) => {
                        unreachable!("[internal error] somehow found an invalid stack keyword in the stack assistant")
                    },
                    _ => self.encode_stack_keyword(keyword, &mut ctx, span),
                },
            }
        }

        if let Some(StackModifier::Post(idents, span)) =
            environment.modifiers.iter().find(|m| matches!(m, StackModifier::Post(_, _)))
        {
            self.encode_postcondition(idents, &mut ctx, span);
        }

        (ctx.solver, ctx.constraints)
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

    fn encode_postcondition(&self, post: &[String], ctx: &mut EncodingContext, span: &Span) {
        let len = post.len() as i64;

        for (j, name) in post.iter().enumerate() {
            // we get the value in the current stack that corresponds to that postcondition identifier
            let slot_j = ctx.read_stack_at(len - j as i64);
            let q = Bool::new_const(format!("post_{}", ctx.next_post));

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

            ctx.constraints.push(TrackedConstraint { literal: q, span: *span });
            ctx.next_post += 1;
        }
    }

    fn encode_stack_keyword(&self, keyword: &str, ctx: &mut EncodingContext, span: &Span) {
        // stack keywords simply simulate the operation on a new z3 array, then commit that array as a new step
        match keyword {
            "@pop" => {
                let next = ctx.state.clone();
                ctx.commit(next, span);
                ctx.sp -= 1;
            },
            "@dup" => {
                let next = ctx.state.store(&Int::from_i64(ctx.sp), &ctx.read_stack_at(1));
                ctx.commit(next, span);
                ctx.sp += 1;
            },
            "@swap" => {
                let (a, b) = (ctx.read_stack_at(2), ctx.read_stack_at(1));
                let next = ctx.state.store(&Int::from_i64(ctx.sp - 2), &b).store(&Int::from_i64(ctx.sp - 1), &a);
                ctx.commit(next, span);
            },
            "@pop_all" => {
                let next = ctx.state.clone();
                ctx.commit(next, span);
                ctx.sp = 0;
            },
            "@over" => {
                let a = ctx.read_stack_at(2);
                let next = ctx.state.store(&Int::from_i64(ctx.sp), &a);
                ctx.sp += 1;
                ctx.commit(next, span);
            },
            "@rot" => {
                let (a, b, c) = (ctx.read_stack_at(3), ctx.read_stack_at(2), ctx.read_stack_at(1));
                let next = ctx
                    .state
                    .store(&Int::from_i64(ctx.sp - 3), &b)
                    .store(&Int::from_i64(ctx.sp - 2), &c)
                    .store(&Int::from_i64(ctx.sp - 1), &a);
                ctx.commit(next, span);
            },
            "@rrot" => {
                let (a, b, c) = (ctx.read_stack_at(3), ctx.read_stack_at(2), ctx.read_stack_at(1));
                let next = ctx
                    .state
                    .store(&Int::from_i64(ctx.sp - 3), &c)
                    .store(&Int::from_i64(ctx.sp - 2), &a)
                    .store(&Int::from_i64(ctx.sp - 1), &b);
                ctx.commit(next, span);
            },
            "@nip" => {
                let b = ctx.read_stack_at(1);
                let next = ctx.state.store(&Int::from_i64(ctx.sp - 2), &b);
                ctx.sp -= 1;
                ctx.commit(next, span);
            },
            "@tuck" => {
                let (a, b) = (ctx.read_stack_at(2), ctx.read_stack_at(1));
                let next = ctx
                    .state
                    .store(&Int::from_i64(ctx.sp - 2), &b)
                    .store(&Int::from_i64(ctx.sp - 1), &a)
                    .store(&Int::from_i64(ctx.sp), &b);
                ctx.sp += 1;
                ctx.commit(next, span);
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

                ctx.commit(next, span);
            },
            "@depth" => {
                let next = ctx.state.store(&Int::from_i64(ctx.sp), &Int::from_i64(ctx.next_id));

                ctx.sp += 1;
                ctx.next_id += 1;
                ctx.commit(next, span);
            },
            _ => panic!("[internal error] found invalid stack keyword when generating encoding"),
        }
    }

    fn get_error_locations(&self, muses: &[Vec<usize>], constraints: &[TrackedConstraint]) -> Vec<Vec<Span>> {
        muses.iter().map(|mus| mus.iter().map(|&i| constraints[i].span).collect()).collect()
    }
}
