use std::path::PathBuf;

use crate::{
    hfs::{
        Span,
        error::{CompileError, DebugInfo, Label, Snippet},
    },
    impl_display_for_error,
};

#[derive(Debug)]
pub enum StackAssistErrorKind {
    NoPostconditionFound,
    AssertionFailed,
}

#[derive(Debug)]
pub struct StackAssistError {
    pub kind: StackAssistErrorKind,
    pub path: PathBuf,
    pub spans: Vec<Span>,
    pub debug_info: DebugInfo,
}

#[macro_export]
macro_rules! stack_assist_error {
    ($kind:expr, $assistant:expr, $span:expr) => {
        Err(Box::new(StackAssistError {
            kind: $kind,
            path: $assistant.diagnostic_info.path.clone(),
            spans: $span,
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
    fn message(&self) -> String {
        match self.kind {
            StackAssistErrorKind::NoPostconditionFound => {
                format!("no postcondition found for this stack environment with a precondition")
            },
            StackAssistErrorKind::AssertionFailed => "stack assertion failed".to_string(),
        }
    }

    fn get_debug_info(&self) -> DebugInfo {
        self.debug_info.clone()
    }

    fn snippets(&self) -> Vec<Snippet> {
        let label_text = match &self.kind {
            StackAssistErrorKind::NoPostconditionFound => None,
            StackAssistErrorKind::AssertionFailed => Some("these expressions conflict with the post condition"),
        };

        let mut spans = self.spans.clone();
        spans.sort_by_key(|s| (s.start.line, s.start.col));

        let last = spans.len().saturating_sub(1);
        let labels = spans
            .iter()
            .enumerate()
            .map(|(i, span)| Label { span: *span, message: if i == last { label_text.map(String::from) } else { None } })
            .collect();

        vec![Snippet { path: self.path.clone(), labels }]
    }
}

impl_display_for_error!(StackAssistError);
