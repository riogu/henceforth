use std::{
    env,
    error::Error,
    fmt::{Debug, Display},
    fs,
    io::Read,
    path::PathBuf,
};

use colored::{ColoredString, Colorize};

use crate::hfs::{File, SourceInfo};

pub trait CompileError: Display + Debug {
    fn message(&self) -> String;
    fn header(&self) -> ColoredString;
    fn location(&self) -> ColoredString;
    fn source_code(&self) -> Result<ColoredString, Box<dyn Error>>;
}

#[derive(Debug)]
pub enum LexerErrorKind {
    UnexpectedChar,
    UnexpectedEof,
}

#[derive(Debug)]
pub struct LexerError {
    kind: LexerErrorKind,
    path: PathBuf,
    source_info: SourceInfo,
}

impl LexerError {
    pub fn new(kind: LexerErrorKind, path: PathBuf, source_info: SourceInfo) -> Self {
        Self { kind, path, source_info }
    }
}

pub fn number_length(n: usize) -> usize {
    n.to_string().len()
}

impl CompileError for LexerError {
    fn header(&self) -> ColoredString {
        format!("{} {}", "error:".red().bold(), self.message().bold()).into()
    }

    fn location(&self) -> ColoredString {
        format!(
            " {} {}:{}:{}",
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
            .ok_or_else(|| format!("Line {} not found in file", self.source_info.line_number))?;

        let mut error_pointer = " ".repeat(self.source_info.line_offset - 1);
        error_pointer.push_str(format!("{} unexpected character", "^".repeat(self.source_info.token_width)).as_str());
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

    fn message(&self) -> String {
        match self.kind {
            LexerErrorKind::UnexpectedChar => String::from("unexpected message"),
            LexerErrorKind::UnexpectedEof => String::from("unexpected EOF"),
        }
    }
}

impl<'a> Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_code = match self.source_code() {
            Ok(source_code) => source_code,
            Err(e) => ColoredString::from(format!("<source unavailable: {}>", e)),
        };
        write!(f, "{}\n{}\n{}", self.header(), self.location(), source_code)
    }
}
