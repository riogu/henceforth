use std::{
    env,
    error::Error,
    fmt::{write, Debug, Display},
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

pub fn number_length(n: usize) -> usize {
    n.to_string().len()
}
