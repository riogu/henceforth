use std::{
    env,
    error::Error,
    fmt::{Debug, Display},
    fs,
    io::Read,
    path::PathBuf,
};

use colored::{ColoredString, Colorize};

use crate::hfs::{File, SourceInfo, Token, TokenKind, VALID_STACK_KEYWORDS};

pub trait CompileError: Display + Debug {
    fn message(&self) -> (String, String);
    fn header(&self) -> ColoredString;
    fn location(&self) -> ColoredString;
    fn source_code(&self) -> Result<ColoredString, Box<dyn Error>>;
}

pub struct DiagnosticInfo {
    pub path: PathBuf,
    pub eof_pos: SourceInfo,
}

impl DiagnosticInfo {
    pub fn new(path: PathBuf, tokens: &Vec<Token>) -> Self {
        let eof_pos = if tokens.len() > 0 {
            let last = tokens.last().expect("[internal error] no tokens after len > 0 check");
            SourceInfo::new(last.source_info.line_number, last.source_info.line_offset + last.source_info.token_width, 1)
        } else {
            SourceInfo::new(1, 1, 1)
        };
        Self { path, eof_pos }
    }
}

#[derive(Debug)]
pub enum LexerErrorKind {
    UnexpectedChar,
    UnexpectedEof,
    InvalidStackKeyword,
}

#[derive(Debug)]
pub struct LexerError {
    kind: LexerErrorKind,
    path: PathBuf,
    source_info: SourceInfo,
}

impl LexerError {
    pub fn new(kind: LexerErrorKind, path: PathBuf, source_info: SourceInfo) -> Result<Vec<Token>, Box<dyn CompileError>> {
        Err(Box::new(Self { kind, path, source_info }))
    }
}

pub fn number_length(n: usize) -> usize {
    n.to_string().len()
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
            .ok_or_else(|| format!("Line {} not found in file", self.source_info.line_number))?;

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
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_code = match self.source_code() {
            Ok(source_code) => source_code,
            Err(e) => ColoredString::from(format!("<source unavailable: {}>", e)),
        };
        write!(f, "{}\n{}\n{}", self.header(), self.location(), source_code)
    }
}

#[derive(Debug)]
pub enum Expectable {
    Token(TokenKind),
    Identifier,
    StackKeyword,
    Type,
}

impl Display for Expectable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expectable::Token(token_kind) => write!(f, "{}", token_kind),
            Expectable::Identifier => write!(f, "identifier"),
            Expectable::StackKeyword => write!(f, "stack keyword"),
            Expectable::Type => write!(f, "type"),
        }
    }
}

#[derive(Debug)]
pub enum ParserErrorKind {
    ExpectedButFound(Expectable, Option<TokenKind>),
}

#[derive(Debug)]
pub struct ParserError {
    kind: ParserErrorKind,
    path: PathBuf,
    source_info: Vec<SourceInfo>,
}

impl ParserError {
    pub fn new<T>(kind: ParserErrorKind, path: PathBuf, source_info: Vec<SourceInfo>) -> Result<T, Box<dyn CompileError>> {
        Err(Box::new(Self { kind, path, source_info }))
    }
}

impl CompileError for ParserError {
    fn message(&self) -> (String, String) {
        match &self.kind {
            ParserErrorKind::ExpectedButFound(expected, found) => match found {
                Some(found) => (format!("expected {}, found {}", expected, found), String::new()),
                None => (format!("expected {}", expected), String::new()),
            },
        }
    }

    fn header(&self) -> ColoredString {
        format!("{} {}", "error:".red().bold(), self.message().0.bold()).into()
    }

    fn location(&self) -> ColoredString {
        format!(
            "{}{} {}:{}:{}",
            " ".repeat(number_length(self.source_info[0].line_number)),
            "-->".blue(),
            self.path.to_str().unwrap(),
            self.source_info[0].line_number,
            self.source_info[0].line_offset
        )
        .into()
    }

    fn source_code(&self) -> Result<ColoredString, Box<dyn Error>> {
        let source = fs::read_to_string(&self.path).map_err(|e| format!("Could not read source file: {}", e))?;

        let line = source
            .lines()
            .nth(self.source_info[0].line_number - 1)
            .ok_or_else(|| format!("Line {} not found in file", self.source_info[0].line_number))?;

        let mut error_pointer = " ".repeat(self.source_info[0].line_offset - 1);
        error_pointer.push_str(format!("{} {}", "^".repeat(self.source_info[0].token_width), self.message().1).as_str());
        return Ok(ColoredString::from(format!(
            "{} {}\n{} {} {}\n{} {} {}",
            " ".repeat(number_length(self.source_info[0].line_number)),
            "|".blue().bold(),
            self.source_info[0].line_number.to_string().blue().bold(),
            "|".blue().bold(),
            line,
            " ".repeat(number_length(self.source_info[0].line_number)),
            "|".blue().bold(),
            error_pointer.red().bold()
        )));
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_code = match self.source_code() {
            Ok(source_code) => source_code,
            Err(e) => ColoredString::from(format!("<source unavailable: {}>", e)),
        };
        write!(f, "{}\n{}\n{}", self.header(), self.location(), source_code)
    }
}
