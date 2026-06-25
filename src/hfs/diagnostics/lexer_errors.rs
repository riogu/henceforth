use std::{
    error::Error,
    fmt::{self, Display},
    fs,
    path::PathBuf,
};

use colored::{ColoredString, Colorize};

use crate::hfs::{
    Span, Token, VALID_STACK_KEYWORDS,
    error::{CompileError, DebugInfo, number_length},
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
    fn message(&self) -> (String, String) {
        match self.kind {
            LexerErrorKind::UnexpectedChar => (String::from("unexpected character"), String::from("unexpected character")),
            LexerErrorKind::UnexpectedEof => (String::from("unexpected EOF"), String::from("unexpected EOF")),
            LexerErrorKind::InvalidStackKeyword => (
                format!(
                    "invalid stack keyword, the valid keywords are {}",
                    VALID_STACK_KEYWORDS.iter().map(|kw| format!("`{}`", kw)).collect::<Vec<String>>().join(", ")
                ),
                String::new(),
            ),
        }
    }
    fn get_span(&self) -> Span { self.span }
    fn get_path(&self) -> PathBuf { self.path.clone() }
    fn get_debug_info(&self) -> DebugInfo { self.debug_info.clone() }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_code = match self.source_code() {
            Ok(source_code) => source_code,
            Err(e) => ColoredString::from(format!("<source unavailable: {}>", e)),
        };

        write!(f, "{}\n{}\n{}{}", self.header(), self.location(), source_code, self.debug_info())
    }
}

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
