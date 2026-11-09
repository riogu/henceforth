use std::{
    fmt::{self, Display},
    path::PathBuf,
};
use crate::{
    hfs::{
        Span, Token, VALID_STACK_KEYWORDS,
        error::{CompileError, DebugInfo, Label, Snippet},
    },
    impl_display_for_error,
};

#[derive(Debug)]
pub enum LexerErrorKind {
    UnexpectedChar,
    UnexpectedEof,
    InvalidStackKeyword,
}

#[derive(Debug)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub path: PathBuf,
    pub span: Span,
    pub debug_info: DebugInfo,
}

#[macro_export]
macro_rules! lexer_error {
    ($kind:expr, $path:expr, $span:expr, $tokens:expr) => {
        Err(Box::new(LexerError {
            kind: $kind,
            path: $path,
            span: $span,
            debug_info: $crate::hfs::diagnostics::error::DebugInfo {
                compiler_file: file!(),
                compiler_line: line!(),
                compiler_column: column!(),
                internal_dump: LexerError::dump_tokens($tokens),
            },
        }))
    };
}

impl LexerError {
    pub fn dump_tokens(tokens: Vec<Token>) -> String {
        format!("[\n{}\n]", tokens.iter().map(|tkn| format!("\t{}", tkn)).collect::<Vec<String>>().join(",\n"))
    }
}

impl CompileError for LexerError {
    fn message(&self) -> String {
        match self.kind {
            LexerErrorKind::UnexpectedChar => String::from("unexpected character"),
            LexerErrorKind::UnexpectedEof => String::from("unexpected EOF"),
            LexerErrorKind::InvalidStackKeyword => format!(
                "invalid stack keyword, the valid keywords are {}",
                VALID_STACK_KEYWORDS.iter().map(|kw| format!("`{}`", kw)).collect::<Vec<String>>().join(", ")
            ),
        }
    }
    fn get_debug_info(&self) -> DebugInfo {
        self.debug_info.clone()
    }

    fn snippets(&self) -> Vec<Snippet> {
        vec![Snippet {
            path: self.path.clone(),
            labels: vec![Label {
                span: self.span,
                message: match self.kind {
                    LexerErrorKind::UnexpectedChar => Some("unexpected character".into()),
                    LexerErrorKind::UnexpectedEof => Some("unexpected EOF".into()),
                    LexerErrorKind::InvalidStackKeyword => None,
                },
            }],
        }]
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl_display_for_error!(LexerError);

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} (pos: {}:{}, width: {})",
            self.kind,
            self.span.start.line,
            self.span.start.col,
            self.span.end.col - self.span.start.col
        )
    }
}
