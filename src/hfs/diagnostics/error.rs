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
    fn header(&self) -> ColoredString;
    fn location(&self) -> ColoredString;
    fn source_code(&self) -> Result<ColoredString, Box<dyn Error>>;
}

#[derive(Debug)]
pub enum LexerError {
    UnexpectedChar { path: PathBuf, source_info: SourceInfo },
    UnexpectedEof { path: PathBuf, source_info: SourceInfo },
}

pub fn number_length(n: usize) -> usize {
    n.to_string().len()
}

impl CompileError for LexerError {
    fn header(&self) -> ColoredString {
        match self {
            LexerError::UnexpectedChar { path, .. } =>
                format!("{} {}", "error:".red().bold(), "unexpected character".bold()).into(),
            LexerError::UnexpectedEof { path, .. } => format!("{} {}", "error:".red().bold(), "unexpected EOF".bold()).into(),
        }
    }

    fn location(&self) -> ColoredString {
        match self {
            LexerError::UnexpectedChar { path, source_info } => {
                let colored_string = format!(
                    " {} {}:{}:{}",
                    "-->".blue(),
                    path.to_str().unwrap(),
                    source_info.line_number,
                    source_info.line_offset
                )
                .into();
                colored_string
            },
            LexerError::UnexpectedEof { path, source_info } => {
                let colored_string = format!(
                    " {} {}:{}:{}",
                    "-->".blue(),
                    path.to_str().unwrap(),
                    source_info.line_number,
                    source_info.line_offset
                )
                .into();
                colored_string
            },
        }
    }

    fn source_code(&self) -> Result<ColoredString, Box<dyn Error>> {
        match self {
            LexerError::UnexpectedChar { path, source_info } => {
                let source = fs::read_to_string(path).map_err(|e| format!("Could not read source file: {}", e))?;

                let line = source
                    .lines()
                    .nth(source_info.line_number - 1)
                    .ok_or_else(|| format!("Line {} not found in file", source_info.line_number))?;

                let mut error_pointer = " ".repeat(source_info.line_offset - 1);
                error_pointer.push_str("^ unexpected character");
                return Ok(ColoredString::from(format!(
                    "{} {}\n{} {} {}\n{} {} {}",
                    " ".repeat(number_length(source_info.line_number)),
                    "|".blue().bold(),
                    source_info.line_number.to_string().blue().bold(),
                    "|".blue().bold(),
                    line,
                    " ".repeat(number_length(source_info.line_number)),
                    "|".blue().bold(),
                    error_pointer.red().bold()
                )));
            },
            LexerError::UnexpectedEof { path, source_info } => {
                let source = fs::read_to_string(path).map_err(|e| format!("Could not read source file: {}", e))?;

                let line = source
                    .lines()
                    .nth(source_info.line_number - 1)
                    .ok_or_else(|| format!("Line {} not found in file", source_info.line_number))?;

                let mut error_pointer = " ".repeat(source_info.line_offset - 1);
                error_pointer.push_str("^ unexpected EOF");
                return Ok(ColoredString::from(format!(
                    "{} {}\n{} {} {}\n{} {} {}",
                    " ".repeat(number_length(source_info.line_number)),
                    "|".blue().bold(),
                    source_info.line_number.to_string().blue().bold(),
                    "|".blue().bold(),
                    line,
                    " ".repeat(number_length(source_info.line_number)),
                    "|".blue().bold(),
                    error_pointer.red().bold()
                )));
            },
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
