use std::collections::HashMap;
use std::rc::Rc;

use z3::{SatResult, Solver};

use crate::hfs::error::{CompileError, DiagnosticInfo};
use crate::hfs::{Expression, Span, StackBlockExpression, StackEnvironment, StackModifier};
use crate::stack_assist_error;
use crate::tools::hfs_assert::stack_assist_errors::{StackAssistError, StackAssistErrorKind};

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
            (None, None) => unreachable!(), // this should never be reached since we check for a pre or post before entering the stack assistant,
            (None, Some(m)) => Ok(StackEnvironment {
                modifiers: vec![StackModifier::Pre(vec![]), m.clone()],
                resolved_body: environment.resolved_body,
                span: environment.span,
            }),
            (Some(m), None) => return stack_assist_error!(StackAssistErrorKind::NoPostconditionFound, &self, environment.span),
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
        let solver = Solver::new();

        if let Some(StackModifier::Pre(idents)) = environment.modifiers.iter().find(|m| matches!(m, StackModifier::Pre(_))) {
            let s0 = self.generate_precondition_variables_and_constraints(idents, &solver);
            dbg!(s0);
        }
        dbg!(solver.get_assertions());

        // for expr in environment.resolved_body.iter() {
        //     match expr {
        //         StackBlockExpression::Expr(expression) => match expression {
        //             Expression::Operation(operation) => todo!(),
        //             Expression::Identifier(identifier) => todo!(),
        //             Expression::Literal(literal) => todo!(),
        //             Expression::Tuple { expressions } => todo!(),
        //             Expression::ArrayLiteral { elements } => todo!(),
        //             Expression::Parameter { index, type_id } => todo!(),
        //             Expression::ReturnValue(type_id) => todo!(),
        //         },
        //         StackBlockExpression::StackKeyword(keyword) => todo!(),
        //     }
        // }
        solver
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

    fn generate_precondition_variables_and_constraints(&self, idents: &[String], solver: &Solver) -> Vec<z3::ast::Int> {
        let mut vars: HashMap<String, z3::ast::Int> = HashMap::new();
        let mut initial_stack: Vec<z3::ast::Int> = vec![];
        for ident in idents {
            let vars_len = vars.len();
            let v = vars.entry(ident.clone()).or_insert_with(|| {
                let tag = vars_len as i64;
                let v = z3::ast::Int::new_const(format!("{ident}"));
                solver.assert(&v.eq(&z3::ast::Int::from_i64(tag)));
                v
            });
            initial_stack.push(v.clone());
        }
        return initial_stack
    }
}
