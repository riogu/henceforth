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

#[derive(Debug, Clone)]
pub enum StackAssistSpan {
    PostCondition(Span),
    StackOperation(Span),
    RemovalHint(Span),
    ModificationHint(Span),
    AdditionHint(Span),
    RegularSpan(Span),
}

#[derive(Debug)]
pub struct StackAssistError {
    pub kind: StackAssistErrorKind,
    pub path: PathBuf,
    pub spans: Vec<StackAssistSpan>,
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
        let sorted = |pick: fn(&StackAssistSpan) -> Option<Span>| -> Vec<Span> {
            let mut spans: Vec<Span> = self.spans.iter().filter_map(pick).collect();
            spans.sort_by_key(|s| (s.start.line, s.start.col));
            spans
        };

        let post_spans = sorted(|s| match s {
            StackAssistSpan::PostCondition(span) => Some(*span),
            _ => None,
        });
        let op_spans = sorted(|s| match s {
            StackAssistSpan::StackOperation(span) => Some(*span),
            _ => None,
        });

        let to_labels = |spans: Vec<Span>, text: Option<&str>| -> Vec<Label> {
            let last = spans.len().saturating_sub(1);
            spans
                .into_iter()
                .enumerate()
                .map(|(i, span)| Label { span, message: if i == last { text.map(String::from) } else { None } })
                .collect()
        };

        let (post_text, op_text) = match &self.kind {
            StackAssistErrorKind::NoPostconditionFound => (None, None),
            StackAssistErrorKind::AssertionFailed => {
                (Some("postcondition does not hold"), Some("these expressions conflict with the postcondition"))
            },
        };

        [(post_spans, post_text), (op_spans, op_text)]
            .into_iter()
            .filter(|(spans, _)| !spans.is_empty())
            .map(|(spans, text)| Snippet { path: self.path.clone(), labels: to_labels(spans, text) })
            .collect()
    }

    fn get_span(&self) -> Span {
        match self.spans.first() {
            Some(first) => match first {
                StackAssistSpan::PostCondition(span) => *span,
                StackAssistSpan::StackOperation(span) => *span,
                StackAssistSpan::RemovalHint(span) => *span,
                StackAssistSpan::ModificationHint(span) => *span,
                StackAssistSpan::AdditionHint(span) => *span,
                StackAssistSpan::RegularSpan(span) => *span,
            },
            None => Span::default(),
        }
    }
}

impl_display_for_error!(StackAssistError);
