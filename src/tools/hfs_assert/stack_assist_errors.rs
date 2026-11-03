use std::{fmt::Display, path::PathBuf};

use colored::ColoredString;

use crate::hfs::{Span, error::{CompileError, DebugInfo}};

#[derive(Debug)]
pub enum StackAssistErrorKind {
    NoPostconditionFound,
    AssertionFailed(Span, String)
}

#[derive(Debug)]
pub struct StackAssistError {
    pub kind: StackAssistErrorKind,
    pub path: PathBuf,
    pub span: Span,
    pub debug_info: DebugInfo,
}

impl Display for StackAssistError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_code = match self.source_code() {
            Ok(source_code) => source_code,
            Err(e) => ColoredString::from(format!("<source unavailable: {}>", e)),
        };
        write!(f, "{}\n{}\n{}{}", self.header(), self.location(), source_code, self.debug_info())
    }
}

#[macro_export]
macro_rules! stack_assist_error {
    ($kind:expr, $assistant:expr, $span:expr) => {
        Err(Box::new(StackAssistError {
            kind: $kind,
            path: $assistant.diagnostic_info.path.clone(),
            span: $span,
            debug_info: $crate::hfs::diagnostics::error::DebugInfo {
                compiler_file: file!(),
                compiler_line: line!(),
                compiler_column: column!(),
                internal_dump: String::new(),
            },
        }))
    };
}


impl CompileError for StackAssistError {
    fn get_span(&self) -> Span {
        self.span
    }

    fn get_path(&self) -> PathBuf {
        self.path.clone()
    }

    fn message(&self) -> (String, String) {
        match self.kind {
            StackAssistErrorKind::NoPostconditionFound => (format!("no postcondition found for this stack environment with a precondition"), String::new()),
            StackAssistErrorKind::AssertionFailed(span, _) => todo!(),
        }
    }

    fn get_debug_info(&self) -> DebugInfo {
        self.debug_info.clone()
    }
}
