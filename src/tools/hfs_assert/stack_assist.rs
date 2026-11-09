use std::collections::HashMap;
use std::rc::Rc;

use z3::ast::{Array, Bool, Dynamic, Int};
use z3::{FuncDecl, SatResult, Solver, Sort, Symbol};

use crate::hfs::error::{CompileError, DiagnosticInfo};
use crate::hfs::{
    Expression, Identifier, Operation, Span, StackBlockExpression, StackEnvironment, StackModifier, VALID_STACK_KEYWORDS,
};
use crate::stack_assist_error;
use crate::tools::hfs_assert::marco_enumerator::MarcoEnumerator;
use crate::tools::hfs_assert::stack_assist_errors::{StackAssistError, StackAssistErrorKind};

#[derive(Clone)]
pub struct Value(Dynamic);

impl Value {
    pub fn new(ast: Dynamic) -> Self {
        Self(ast)
    }

    pub fn as_ast(&self) -> &Dynamic {
        &self.0
    }
}

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
    pub value_sort: Sort,
    pub max_possible_stack_depth: i64,
    pub next_id: i64,
    pub names: HashMap<String, Value>,
    pub depth_offset: i64,
    pub depth_fn: FuncDecl,
    pub depth_values: Vec<(Int, Value)>,
    pub constraints: Vec<TrackedConstraint>, // assumption literals, used to track which step is part of the MUS
    pub next_step: usize,
    pub next_post: usize,
    pub literal_values: HashMap<String, Value>,
    pub op_fns: HashMap<&'static str, FuncDecl>,
}

impl EncodingContext {
    pub fn new() -> Self {
        let value_sort = Sort::uninterpreted(Symbol::from("Value"));
        let initial_sp = Int::new_const("initial_sp");
        Self {
            solver: Solver::new(),
            state: Array::new_const("s0", &Sort::int(), &value_sort),
            sp: initial_sp.clone(),
            next_id: 0,
            max_possible_stack_depth: 0,
            names: HashMap::new(),
            constraints: vec![],
            next_step: 0,
            next_post: 0,
            initial_sp,
            depth_offset: 0,
            literal_values: HashMap::new(),
            depth_fn: FuncDecl::new("depth", &[&Sort::int()], &value_sort),
            value_sort,
            depth_values: vec![],
            op_fns: HashMap::new(),
        }
    }

    pub fn push_to_stack(&mut self, value: &Value, span: &Span) {
        let next = self.state.store(&self.sp, value.as_ast());
        let next_sp = &self.sp + Int::from_i64(1);
        self.max_possible_stack_depth += 1;

        let validity = self.sp.ge(&Int::from_i64(0));
        self.commit(next, next_sp, validity, span);
    }

    pub fn read_stack_at(&self, d: i64) -> Value {
        let ast = self.state.select(&(&self.sp - Int::from_i64(d)));

        Value::new(ast)
    }

    pub fn commit(&mut self, next: Array, next_sp: Int, validity: Bool, span: &Span) {
        let k = self.next_step;
        let p = Bool::new_const(format!("step_{k}"));
        let previous_state = self.state.clone();
        let previous_sp = self.sp.clone();
        let new = Array::new_const(format!("s{}", k + 1), &Sort::int(), &self.value_sort);

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

    pub fn fresh_value(&mut self) -> Value {
        let id = self.next_id;
        self.next_id += 1;

        Value::new(Dynamic::new_const(format!("value_{id}"), &self.value_sort))
    }

    pub fn get_or_create_value(&mut self, key: String) -> Value {
        if let Some(value) = self.literal_values.get(&key) {
            return value.clone();
        }

        let value = self.fresh_value();
        self.literal_values.insert(key, value.clone());
        value
    }

    fn apply_op(&mut self, name: &'static str, args: &[&Value]) -> Value {
        if !self.op_fns.contains_key(name) {
            let domain: Vec<&Sort> = args.iter().map(|_| &self.value_sort).collect();
            let f = FuncDecl::new(name, &domain, &self.value_sort);
            self.op_fns.insert(name, f);
        }

        let f = &self.op_fns[name];
        let asts: Vec<&dyn z3::ast::Ast> = args.iter().map(|v| v.as_ast() as &dyn z3::ast::Ast).collect();
        Value::new(f.apply(&asts))
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

        dbg!(&solver.get_assertions());
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
                            StackAssistErrorKind::AssertionFailed,
                            &self,
                            self.get_error_locations(&[loc.clone()], &constraints).first().unwrap().clone()
                        );
                    },

                    None => {
                        return stack_assist_error!(
                            StackAssistErrorKind::AssertionFailed,
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
        if let Some(StackModifier::Pre(idents, _)) = environment.modifiers.iter().find(|m| matches!(m, StackModifier::Pre(_, _)))
        {
            self.encode_precondition(idents, &mut ctx);
        }

        for expr in environment.resolved_body.iter() {
            match expr {
                StackBlockExpression::Expr(expression, span) => match expression {
                    Expression::Operation(operation) => self.encode_operation(operation, &mut ctx, span),
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
        let mut previous: Vec<(&String, Value)> = Vec::new();

        for ident in idents {
            let value = ctx.get_or_create_value(format!("precondition:{ident}"));

            // distinct precondition identifiers represent distinct values
            for (prev_name, prev_value) in &previous {
                if *prev_name != ident {
                    ctx.solver.assert(&value.as_ast().eq(prev_value.as_ast()).not());
                }
            }

            let index = ctx.sp.clone();
            ctx.state = ctx.state.store(&index, value.as_ast());

            ctx.names.insert(ident.clone(), value.clone());
            previous.push((ident, value));

            ctx.max_possible_stack_depth += 1;
            ctx.sp = &ctx.sp + Int::from_i64(1);
        }
    }

    fn encode_postcondition(&self, post: &[String], ctx: &mut EncodingContext, span: &Span) {
        let len = post.len() as i64;

        for (j, name) in post.iter().enumerate() {
            // we get the value in the current stack that corresponds to that postcondition identifier
            let slot_j = ctx.read_stack_at(len - j as i64);
            let q = Bool::new_const(format!("post_{}", ctx.next_post));
            let required_sp = &ctx.initial_sp + Int::from_i64(len);
            ctx.solver.assert(&q.implies(&ctx.sp.ge(&required_sp)));

            // if a name from the pre pins this slot to that pre value, we assert equality between them
            if let Some(value) = ctx.names.get(name) {
                ctx.solver.assert(&q.implies(&slot_j.as_ast().eq(value.as_ast())));
            }

            // same name => same value, different name => different value
            for i in 0..j {
                let slot_i = ctx.read_stack_at(len - i as i64);
                let same = slot_j.as_ast().eq(slot_i.as_ast());
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
                let next = ctx.state.store(&ctx.sp, ctx.read_stack_at(1).as_ast());
                let next_sp = &ctx.sp + Int::from_i64(1);
                ctx.max_possible_stack_depth += 1;

                let validity = ctx.sp.ge(&Int::from_i64(1));
                ctx.commit(next, next_sp, validity, span);
            },

            "@swap" => {
                let (a, b) = (ctx.read_stack_at(2), ctx.read_stack_at(1));

                let index_a = &ctx.sp - Int::from_i64(2);
                let index_b = &ctx.sp - Int::from_i64(1);

                let next = ctx.state.store(&index_a, b.as_ast()).store(&index_b, a.as_ast());

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

                let next = ctx.state.store(&ctx.sp, a.as_ast());
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

                let next = ctx.state.store(&index_a, b.as_ast()).store(&index_b, c.as_ast()).store(&index_c, a.as_ast());

                let next_sp = ctx.sp.clone();

                let validity = ctx.sp.ge(&Int::from_i64(3));
                ctx.commit(next, next_sp, validity, span);
            },

            "@rrot" => {
                let (a, b, c) = (ctx.read_stack_at(3), ctx.read_stack_at(2), ctx.read_stack_at(1));

                let index_a = &ctx.sp - Int::from_i64(3);
                let index_b = &ctx.sp - Int::from_i64(2);
                let index_c = &ctx.sp - Int::from_i64(1);

                let next = ctx.state.store(&index_a, c.as_ast()).store(&index_b, a.as_ast()).store(&index_c, b.as_ast());

                let next_sp = ctx.sp.clone();

                let validity = ctx.sp.ge(&Int::from_i64(3));
                ctx.commit(next, next_sp, validity, span);
            },

            "@nip" => {
                let b = ctx.read_stack_at(1);
                let index = &ctx.sp - Int::from_i64(2);

                let next = ctx.state.store(&index, b.as_ast());
                let next_sp = &ctx.sp - Int::from_i64(1);

                let validity = ctx.sp.ge(&Int::from_i64(2));
                ctx.commit(next, next_sp, validity, span);
            },

            "@tuck" => {
                let (a, b) = (ctx.read_stack_at(2), ctx.read_stack_at(1));

                let index_a = &ctx.sp - Int::from_i64(2);
                let index_b = &ctx.sp - Int::from_i64(1);
                ctx.max_possible_stack_depth += 1;

                let next = ctx.state.store(&index_a, b.as_ast()).store(&index_b, a.as_ast()).store(&ctx.sp, b.as_ast());

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

                    let reversed = ctx.state.select(&reverse_index);

                    let original = ctx.state.select(&index);

                    let value = condition.ite(&reversed, &original);

                    next = next.store(&index, &value);
                }

                let next_sp = ctx.sp.clone();

                let validity = ctx.sp.ge(&Int::from_i64(0));
                ctx.commit(next, next_sp, validity, span);
            },

            "@depth" => {
                let depth = &ctx.sp + Int::from_i64(ctx.depth_offset);
                let value = Value::new(ctx.depth_fn.apply(&[&depth]));

                for val in ctx.literal_values.values().chain(ctx.names.values()) {
                    ctx.solver.assert(&value.as_ast().eq(val.as_ast()).not());
                }

                for (prev_depth, prev_value) in &ctx.depth_values {
                    // different depths must produce different values
                    let same_depth = depth.eq(prev_depth);
                    let different_value = value.as_ast().eq(prev_value.as_ast()).not();

                    ctx.solver.assert(&Bool::or(&[&same_depth, &different_value]));
                }

                ctx.depth_values.push((depth.clone(), value.clone()));

                let next = ctx.state.store(&ctx.sp, value.as_ast());
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
        let value = ctx.get_or_create_value(format!("literal:{expression:?}"));
        ctx.push_to_stack(&value, span);
    }

    fn encode_identifier(&self, identifier: &Identifier, ctx: &mut EncodingContext, span: &Span) {
        match identifier {
            Identifier::Variable(_) | Identifier::GlobalVar(_) => {
                let value = &ctx.get_or_create_value(format!("identifier:{identifier:?}"));
                ctx.push_to_stack(&value, span);
            },
            Identifier::Function(_) => panic!("[internal error] function identifiers can't be inside a stack block"),
        }
    }

    fn encode_binary(&self, name: &'static str, commutative: bool, ctx: &mut EncodingContext, span: &Span) {
        let (a, b) = (ctx.read_stack_at(2), ctx.read_stack_at(1));
        let result = ctx.apply_op(name, &[&a, &b]);

        if commutative {
            let swapped = ctx.apply_op(name, &[&b, &a]);
            ctx.solver.assert(&result.as_ast().eq(swapped.as_ast()));
        }

        let index = &ctx.sp - Int::from_i64(2);
        let next = ctx.state.store(&index, result.as_ast());
        let next_sp = &ctx.sp - Int::from_i64(1);
        let validity = ctx.sp.ge(&Int::from_i64(2));
        ctx.commit(next, next_sp, validity, span);
    }

    fn encode_unary(&self, name: &'static str, ctx: &mut EncodingContext, span: &Span) {
        let a = ctx.read_stack_at(1);
        let result = ctx.apply_op(name, &[&a]);

        let index = &ctx.sp - Int::from_i64(1);
        let next = ctx.state.store(&index, result.as_ast());
        let next_sp = ctx.sp.clone();
        let validity = ctx.sp.ge(&Int::from_i64(1));
        ctx.commit(next, next_sp, validity, span);
    }

    fn encode_operation(&self, operation: &Operation, ctx: &mut EncodingContext, span: &Span) {
        match operation {
            Operation::Add(..) => self.encode_binary("add", true, ctx, span),
            Operation::Mul(..) => self.encode_binary("mul", true, ctx, span),
            Operation::Equal(..) => self.encode_binary("eq", true, ctx, span),
            Operation::NotEqual(..) => self.encode_binary("neq", true, ctx, span),
            Operation::And(..) => self.encode_binary("and", true, ctx, span),
            Operation::Or(..) => self.encode_binary("or", true, ctx, span),
            Operation::Sub(..) => self.encode_binary("sub", false, ctx, span),
            Operation::Div(..) => self.encode_binary("div", false, ctx, span),
            Operation::Mod(..) => self.encode_binary("mod", false, ctx, span),
            Operation::Less(..) => self.encode_binary("lt", false, ctx, span),
            Operation::LessEqual(..) => self.encode_binary("le", false, ctx, span),
            Operation::Greater(..) => self.encode_binary("gt", false, ctx, span),
            Operation::GreaterEqual(..) => self.encode_binary("ge", false, ctx, span),
            Operation::ArrayAccess(..) => self.encode_binary("index", false, ctx, span),
            Operation::Not(_) => self.encode_unary("not", ctx, span),
            Operation::AddressOf(_) => self.encode_unary("addr_of", ctx, span),
            Operation::Dereference(_) => self.encode_unary("deref", ctx, span),
        }
    }
}
