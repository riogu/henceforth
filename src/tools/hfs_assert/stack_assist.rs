use std::path::PathBuf;
use std::rc::Rc;

use crate::hfs::error::{CompileError, DiagnosticInfo};
use crate::hfs::{StackEnvironment, StackModifier};
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
        let normalized_environment = self.normalize_environment(environment)?;
        Ok(())
    }
}
