use std::collections::HashMap;
use std::rc::Rc;

use z3::ast::{Array, Bool, Int};
use z3::{SatResult, Solver, Sort};

use crate::hfs::error::{CompileError, DiagnosticInfo};
use crate::hfs::{Expression, Identifier, Span, StackBlockExpression, StackEnvironment, StackModifier, VALID_STACK_KEYWORDS};
use crate::stack_assist_error;
use crate::tools::hfs_assert::marco_enumerator::MarcoEnumerator;
use crate::tools::hfs_assert::stack_assist_errors::{StackAssistError, StackAssistErrorKind};

#[derive(Clone, PartialEq, Debug)]
pub enum ConstraintKind {
    StackOperation,
    PostCondition,
}

#[derive(Clone, Debug)]
pub struct TrackedConstraint {
    pub literal: Bool,
    pub span: Span,
    pub kind: ConstraintKind,
}

pub struct EncodingContext {
    pub solver: Solver,
    pub state: Array,
    pub initial_sp: Int,
    pub sp: Int,
    pub max_possible_stack_depth: i64,
    pub next_id: i64,
    pub names: HashMap<String, i64>,
    pub depth_offset: i64,
    pub constraints: Vec<TrackedConstraint>, // assumption literals, used to track which step is part of the MUS
    pub next_step: usize,
    pub next_post: usize,
    pub literal_ids: HashMap<String, i64>,
}

impl EncodingContext {
    pub fn new() -> Self {
        let initial_sp = Int::new_const("initial_sp");
        Self {
            solver: Solver::new(),
            state: Array::new_const("s0", &Sort::int(), &Sort::int()),
            sp: initial_sp.clone(),
            next_id: 0,
            max_possible_stack_depth: 0,
            names: HashMap::new(),
            constraints: vec![],
            next_step: 0,
            next_post: 0,
            initial_sp,
            depth_offset: 0,
            literal_ids: HashMap::new(),
        }
    }

    pub fn push_to_stack(&mut self, id: i64, span: &Span) {
        let next = self.state.store(&self.sp, &Int::from_i64(id));
        let next_sp = &self.sp + Int::from_i64(1);
        self.max_possible_stack_depth += 1;

        let validity = self.sp.ge(&Int::from_i64(0));
        self.commit(next, next_sp, validity, span);
    }

    pub fn read_stack_at(&self, d: i64) -> Int {
        self.state.select(&(&self.sp - Int::from_i64(d))).as_int().unwrap()
    }

    pub fn commit(&mut self, next: Array, next_sp: Int, validity: Bool, span: &Span) {
        let k = self.next_step;
        let p = Bool::new_const(format!("step_{k}"));
        let previous_state = self.state.clone();
        let previous_sp = self.sp.clone();
        let new = Array::new_const(format!("s{}", k + 1), &Sort::int(), &Sort::int());

        let new_sp = Int::new_const(format!("sp{}", k + 1));
        self.solver.assert(&p.implies(&validity));
        self.solver.assert(&p.implies(&new.eq(&next)));
        self.solver.assert(&p.implies(&new_sp.eq(&next_sp)));

        // to avoid MUSes containing all operations after the one that actually causes the conflict (since it is a sequence of implications), we need to assert that disabling that step implies the state stays still
        self.solver.assert(&p.not().implies(&new.eq(&previous_state)));
        self.solver.assert(&p.not().implies(&new_sp.eq(&previous_sp)));

        self.state = new;
        self.sp = new_sp;
        self.constraints.push(TrackedConstraint { literal: p, span: *span, kind: ConstraintKind::StackOperation });
        self.next_step += 1;
    }

    fn literal_id(&mut self, key: String) -> i64 {
        *self.literal_ids.entry(key).or_insert_with(|| {
            let id = self.next_id;
            self.next_id += 1;
            id
        })
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
            (Some(_), None) => {
                return stack_assist_error!(StackAssistErrorKind::NoPostconditionFound, &self, vec![environment.span]);
            },
            (Some(_), Some(_)) => Ok(environment),
        }
    }

    pub fn find_error_and_correction(&self, environment: StackEnvironment) -> Result<(), Box<dyn CompileError>> {
        let normalized_environment = self.normalize_environment(environment.clone())?;

        let (solver, constraints) = self.generate_variables_and_constraints(&normalized_environment);
        let assumptions: Vec<Bool> = constraints.iter().map(|c| c.literal.clone()).collect();
        if solver.check_assumptions(&assumptions) == SatResult::Sat {
            return Ok(());
        }

        let marco = MarcoEnumerator::new(solver.clone(), constraints.clone());
        let result = marco.enumerate()?;

        if !result.muses.is_empty() {
            let best_mus = result.muses.iter().min_by_key(|mus| {
                let operation_count = mus.iter().filter(|&&i| constraints[i].kind == ConstraintKind::StackOperation).count();

                (operation_count == 0, operation_count)
            });

            let best_mcs = result
                .mcses
                .iter()
                .filter(|mcs| !mcs.is_empty() && mcs.iter().all(|&i| constraints[i].kind == ConstraintKind::StackOperation))
                .min_by_key(|mcs| (mcs.len(), std::cmp::Reverse(mcs.iter().max().copied())));

            dbg!(&result.mcses);
            match best_mus {
                Some(loc) => match best_mcs {
                    Some(mcs) => {
                        dbg!(self.get_error_locations(&[mcs.clone()], &constraints));
                        return stack_assist_error!(
                            StackAssistErrorKind::AssertionFailed(
                                self.get_error_locations(&[mcs.clone()], &constraints).first().unwrap().clone()
                            ),
                            &self,
                            self.get_error_locations(&[loc.clone()], &constraints).first().unwrap().clone()
                        );
                    },

                    None => {
                        return stack_assist_error!(
                            StackAssistErrorKind::AssertionFailed(vec![Span::default()]),
                            &self,
                            self.get_error_locations(&[loc.clone()], &constraints).first().unwrap().clone()
                        );
                    },
                },
                None => panic!("[internal error] could not find best MUS"),
            }
        }

        Ok(())
    }

    fn generate_variables_and_constraints(&self, environment: &StackEnvironment) -> (Solver, Vec<TrackedConstraint>) {
        let mut ctx = EncodingContext::new();

        let pre_len = environment
            .modifiers
            .iter()
            .find_map(|m| match m {
                StackModifier::Pre(idents, _) => Some(idents.len()),
                _ => None,
            })
            .unwrap_or(0);

        ctx.depth_offset = (pre_len + environment.resolved_body.len()) as i64 + 1;

        ctx.solver.assert(&ctx.initial_sp.ge(&Int::from_i64(0)));

        // stack slots below the precondition must have negative ids, to prevent them from conflicting with the actual visible stack
        let post_len = environment
            .modifiers
            .iter()
            .find_map(|m| match m {
                StackModifier::Post(idents, _) => Some(idents.len()),
                _ => None,
            })
            .unwrap_or(0);
        let depth_bound = (3 * environment.resolved_body.len() + post_len) as i64;

        for k in 1..=depth_bound {
            let slot = ctx.state.select(&(&ctx.initial_sp - Int::from_i64(k))).as_int().unwrap();
            ctx.solver.assert(&slot.eq(&Int::from_i64(-k)));
        }

        if let Some(StackModifier::Pre(idents, _)) = environment.modifiers.iter().find(|m| matches!(m, StackModifier::Pre(_, _)))
        {
            self.encode_precondition(idents, &mut ctx);
        }

        for expr in environment.resolved_body.iter() {
            match expr {
                StackBlockExpression::Expr(expression, span) => match expression {
                    Expression::Operation(operation) => todo!(),
                    Expression::Identifier(identifier) => self.encode_identifier(identifier, &mut ctx, span),
                    Expression::Literal(_) | Expression::ArrayLiteral { .. } | Expression::Tuple { .. } => {
                        // TODO: make arrays and tuples use the arena to see if they are equal and intern properly
                        self.encode_value(expression, &mut ctx, span)
                    },
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
            ctx.max_possible_stack_depth += 1;
            ctx.names.insert(ident.clone(), ctx.next_id);
            let index = &ctx.sp;
            ctx.state = ctx.state.store(index, &Int::from_i64(ctx.next_id));
            ctx.sp = &ctx.sp + Int::from_i64(1);
            ctx.next_id += 1;
        }
    }

    fn encode_postcondition(&self, post: &[String], ctx: &mut EncodingContext, span: &Span) {
        let len = post.len() as i64;

        for (j, name) in post.iter().enumerate() {
            // we get the value in the current stack that corresponds to that postcondition identifier
            let slot_j = ctx.read_stack_at(len - j as i64);
            let q = Bool::new_const(format!("post_{}", ctx.next_post));
            ctx.solver.assert(&q.implies(&ctx.sp.ge(&Int::from_i64(len))));

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

            ctx.constraints.push(TrackedConstraint { literal: q, span: *span, kind: ConstraintKind::PostCondition });
            ctx.next_post += 1;
        }
    }

    fn encode_stack_keyword(&self, keyword: &str, ctx: &mut EncodingContext, span: &Span) {
        // stack keywords simply simulate the operation on a new z3 array, then commit that array as a new step
        match keyword {
            "@pop" => {
                let next = ctx.state.clone();
                let next_sp = &ctx.sp - Int::from_i64(1);

                let validity = ctx.sp.ge(&Int::from_i64(1));
                ctx.commit(next, next_sp, validity, span);
            },

            "@dup" => {
                let next = ctx.state.store(&ctx.sp, &ctx.read_stack_at(1));
                let next_sp = &ctx.sp + Int::from_i64(1);
                ctx.max_possible_stack_depth += 1;

                let validity = ctx.sp.ge(&Int::from_i64(1));
                ctx.commit(next, next_sp, validity, span);
            },

            "@swap" => {
                let (a, b) = (ctx.read_stack_at(2), ctx.read_stack_at(1));

                let index_a = &ctx.sp - Int::from_i64(2);
                let index_b = &ctx.sp - Int::from_i64(1);

                let next = ctx.state.store(&index_a, &b).store(&index_b, &a);

                let next_sp = ctx.sp.clone();

                let validity = ctx.sp.ge(&Int::from_i64(2));
                ctx.commit(next, next_sp, validity, span);
            },

            "@pop_all" => {
                let next = ctx.state.clone();
                let next_sp = Int::from_i64(0);

                let validity = ctx.sp.ge(&Int::from_i64(0));
                ctx.commit(next, next_sp, validity, span);
            },

            "@over" => {
                let a = ctx.read_stack_at(2);

                let next = ctx.state.store(&ctx.sp, &a);
                let next_sp = &ctx.sp + Int::from_i64(1);
                ctx.max_possible_stack_depth += 1;

                let validity = ctx.sp.ge(&Int::from_i64(2));
                ctx.commit(next, next_sp, validity, span);
            },

            "@rot" => {
                let (a, b, c) = (ctx.read_stack_at(3), ctx.read_stack_at(2), ctx.read_stack_at(1));

                let index_a = &ctx.sp - Int::from_i64(3);
                let index_b = &ctx.sp - Int::from_i64(2);
                let index_c = &ctx.sp - Int::from_i64(1);

                let next = ctx.state.store(&index_a, &b).store(&index_b, &c).store(&index_c, &a);

                let next_sp = ctx.sp.clone();

                let validity = ctx.sp.ge(&Int::from_i64(3));
                ctx.commit(next, next_sp, validity, span);
            },

            "@rrot" => {
                let (a, b, c) = (ctx.read_stack_at(3), ctx.read_stack_at(2), ctx.read_stack_at(1));

                let index_a = &ctx.sp - Int::from_i64(3);
                let index_b = &ctx.sp - Int::from_i64(2);
                let index_c = &ctx.sp - Int::from_i64(1);

                let next = ctx.state.store(&index_a, &c).store(&index_b, &a).store(&index_c, &b);

                let next_sp = ctx.sp.clone();

                let validity = ctx.sp.ge(&Int::from_i64(3));
                ctx.commit(next, next_sp, validity, span);
            },

            "@nip" => {
                let b = ctx.read_stack_at(1);
                let index = &ctx.sp - Int::from_i64(2);

                let next = ctx.state.store(&index, &b);
                let next_sp = &ctx.sp - Int::from_i64(1);

                let validity = ctx.sp.ge(&Int::from_i64(2));
                ctx.commit(next, next_sp, validity, span);
            },

            "@tuck" => {
                let (a, b) = (ctx.read_stack_at(2), ctx.read_stack_at(1));

                let index_a = &ctx.sp - Int::from_i64(2);
                let index_b = &ctx.sp - Int::from_i64(1);
                ctx.max_possible_stack_depth += 1;

                let next = ctx.state.store(&index_a, &b).store(&index_b, &a).store(&ctx.sp, &b);

                let next_sp = &ctx.sp + Int::from_i64(1);

                let validity = ctx.sp.ge(&Int::from_i64(2));
                ctx.commit(next, next_sp, validity, span);
            },

            "@rev" => {
                let mut next = ctx.state.clone();

                for i in 0..ctx.max_possible_stack_depth {
                    let index = Int::from_i64(i);

                    let condition = index.lt(&ctx.sp);

                    let reverse_index = &ctx.sp - Int::from_i64(1) - Int::from_i64(i);

                    let reversed = ctx.state.select(&reverse_index).as_int().unwrap();

                    let original = ctx.state.select(&index).as_int().unwrap();

                    let value = condition.ite(&reversed, &original);

                    next = next.store(&index, &value);
                }

                let next_sp = ctx.sp.clone();

                let validity = ctx.sp.ge(&Int::from_i64(0));
                ctx.commit(next, next_sp, validity, span);
            },

            "@depth" => {
                let value = &ctx.sp + Int::from_i64(ctx.depth_offset);

                let next = ctx.state.store(&ctx.sp, &value);
                let next_sp = &ctx.sp + Int::from_i64(1);
                ctx.max_possible_stack_depth += 1;

                let validity = ctx.sp.ge(&Int::from_i64(0));
                ctx.commit(next, next_sp, validity, span);
            },

            _ => panic!("[internal error] found invalid stack keyword when generating encoding"),
        }
    }

    fn get_error_locations(&self, muses: &[Vec<usize>], constraints: &[TrackedConstraint]) -> Vec<Vec<Span>> {
        muses.iter().map(|mus| mus.iter().map(|&i| constraints[i].span).collect()).collect()
    }

    fn encode_value(&self, expression: &Expression, ctx: &mut EncodingContext, span: &Span) {
        let id = ctx.literal_id(format!("{expression:?}"));
        ctx.push_to_stack(id, span);
    }

    fn encode_identifier(&self, identifier: &Identifier, ctx: &mut EncodingContext, span: &Span) {
        match identifier {
            Identifier::Variable(_) | Identifier::GlobalVar(_) => {
                let id = ctx.literal_id(format!("{identifier:?}"));
                ctx.push_to_stack(id, span);
            },
            Identifier::Function(_) => panic!("[internal error] function identifiers can't be inside a stack block"),
        }
    }
}
