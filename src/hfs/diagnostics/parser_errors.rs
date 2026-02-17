use std::{error::Error, fmt::Display, fs, path::PathBuf};

use colored::{ColoredString, Colorize};

use crate::hfs::{
    error::{number_length, CompileError},
    SourceInfo, TokenKind,
};

#[derive(Debug)]
pub enum Expectable {
    AnyToken,
    Token(TokenKind),
    Identifier,
    StackKeyword,
    Type,
    VariableDecl,
    FunctionDecl,
    Statement,
    StackExpression,
    StackOperation,
}

impl Display for Expectable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expectable::Token(token_kind) => write!(f, "{}", token_kind),
            Expectable::Identifier => write!(f, "identifier"),
            Expectable::StackKeyword => write!(f, "stack keyword"),
            Expectable::Type => write!(f, "type"),
            Expectable::VariableDecl => write!(f, "variable declaration"),
            Expectable::FunctionDecl => write!(f, "function declaration"),
            Expectable::Statement => write!(f, "statement"),
            Expectable::AnyToken => write!(f, "token"),
            Expectable::StackExpression => write!(f, "stack expression"),
            Expectable::StackOperation => write!(f, "stack operation"),
        }
    }
}

#[derive(Debug)]
pub enum ParserErrorKind {
    ExpectedButFound(Vec<Expectable>, Option<TokenKind>),
}

#[derive(Debug)]
pub struct ParserError {
    kind: ParserErrorKind,
    path: PathBuf,
    source_info: SourceInfo,
}

impl ParserError {
    pub fn new<T>(kind: ParserErrorKind, path: PathBuf, source_infos: Vec<SourceInfo>) -> Result<T, Box<dyn CompileError>> {
        Err(Box::new(Self { kind, path, source_info: Self::merge_source_info(source_infos) }))
    }

    pub fn merge_source_info(mut source_infos: Vec<SourceInfo>) -> SourceInfo {
        source_infos.sort();
        let first = source_infos.first().unwrap();
        let last = source_infos.last().unwrap();

        SourceInfo {
            line_number: first.line_number,
            line_offset: first.line_offset,
            token_width: last.line_offset + last.token_width - first.line_offset,
        }
    }
}

impl CompileError for ParserError {
    fn message(&self) -> (String, String) {
        match &self.kind {
            ParserErrorKind::ExpectedButFound(expected, found) => match found {
                Some(found) => {
                    let expected_repr = format!(
                        "{}",
                        expected.iter().map(|expected| format!("{}", expected)).collect::<Vec<String>>().join(" or ")
                    );
                    (format!("expected {}, found {}", expected_repr, found), String::new())
                },
                None => {
                    let expected_repr = format!(
                        "{}",
                        expected.iter().map(|expected| format!("{}", expected)).collect::<Vec<String>>().join(" or ")
                    );
                    (format!("expected {}", expected_repr), String::new())
                },
            },
        }
    }

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
