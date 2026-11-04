use std::collections::HashMap;
use std::rc::Rc;

use z3::ast::{Array, Int};
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
}

impl EncodingContext {
    pub fn new() -> Self {
        Self {
            solver: Solver::new(),
            state: Array::new_const("so", &Sort::int(), &Sort::int()),
            sp: 0,
            next_id: 0,
            names: HashMap::new(),
        }
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

        let solver = self.generate_variables_and_constraints(&normalized_environment);

        if solver.check() == SatResult::Unsat {
            let core = solver.get_unsat_core();
            let mus = self.minimize_core(core);
            let mus_span = self.get_mus_span(&mus);
            let mcs = self.find_mcs(&mus);
            let correction = self.find_correction(mcs, &normalized_environment);
            return stack_assist_error!(StackAssistErrorKind::AssertionFailed(mus_span, correction), &self, environment.span);
        }
        Ok(())
    }

    fn generate_variables_and_constraints(&self, environment: &StackEnvironment) -> Solver {
        let mut ctx = EncodingContext::new();

        if let Some(StackModifier::Pre(idents)) = environment.modifiers.iter().find(|m| matches!(m, StackModifier::Pre(_))) {
            self.generate_precondition_variables_and_constraints(idents, &mut ctx);
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
                    _ => todo!(),
                },
            }
        }
        ctx.solver
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

    fn generate_precondition_variables_and_constraints(&self, idents: &[String], ctx: &mut EncodingContext) {
        // the precondition never fails so we never add any constraints
        // we just build the initial state and store it in the context, so it can be asserted later
        for ident in idents {
            ctx.names.insert(ident.clone(), ctx.next_id);
            ctx.state = ctx.state.store(&Int::from_i64(ctx.sp), &Int::from_i64(ctx.next_id));
            ctx.sp += 1;
            ctx.next_id += 1;
        }
    }
}
