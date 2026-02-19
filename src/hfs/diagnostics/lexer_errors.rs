use std::{
    error::Error,
    fmt::{self, Display},
    fs,
    path::PathBuf,
};

use colored::{ColoredString, Colorize};

use crate::hfs::{
    error::{number_length, CompileError, DebugInfo},
    SourceInfo, Token, VALID_STACK_KEYWORDS,
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
    pub source_info: SourceInfo,
    pub debug_info: DebugInfo,
}

#[macro_export]
macro_rules! lexer_error {
    ($kind:expr, $path:expr, $source_info:expr, $tokens:expr) => {
        Err(Box::new(LexerError {
            kind: $kind,
            path: $path,
            source_info: $source_info,
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
    fn header(&self) -> ColoredString {
        format!("{} {}", "error:".red().bold(), self.message().0.bold()).into()
    }

    fn location(&self) -> ColoredString {
        format!(
            "{}{} {}:{}:{}",
            " ".repeat(number_length(self.source_info.line_number)),
            "-->".blue(),
            self.path.to_str().unwrap(),
            self.source_info.line_number,
            self.source_info.line_offset
        )
        .into()
    }

    fn source_code(&self) -> Result<ColoredString, Box<dyn Error>> {
        let source = fs::read_to_string(&self.path).map_err(|e| format!("Could not read source file: {}", e))?;

        let line = source
            .lines()
            .nth(self.source_info.line_number - 1)
            .ok_or_else(|| format!("Line {} not found in file", self.source_info.line_number))?
            .replace("\t", "    ");

        let mut error_pointer = " ".repeat(self.source_info.line_offset - 1);
        error_pointer.push_str(format!("{} {}", "^".repeat(self.source_info.token_width), self.message().1).as_str());
        return Ok(ColoredString::from(format!(
            "{} {}\n{} {} {}\n{} {} {}",
            " ".repeat(number_length(self.source_info.line_number)),
            "|".blue().bold(),
            self.source_info.line_number.to_string().blue().bold(),
            "|".blue().bold(),
            line,
            " ".repeat(number_length(self.source_info.line_number)),
            "|".blue().bold(),
            error_pointer.red().bold()
        )));
    }

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

    fn debug_info(&self) -> ColoredString {
        #[cfg(debug_assertions)]
        return ColoredString::from(format!(
            "\n\nDebug info:\n\tprogram crashed at [{} @ {}:{}]\n\nInternal dump:\n{}",
            self.debug_info.compiler_file,
            self.debug_info.compiler_line,
            self.debug_info.compiler_column,
            self.debug_info.internal_dump
        ));

        #[cfg(not(debug_assertions))]
        return ColoredString::from("");
    }
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
            self.kind, self.source_info.line_number, self.source_info.line_offset, self.source_info.token_width
        )
    }
}
