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
    fn debug_info(&self) -> ColoredString;
}

#[derive(Debug, Default, Clone)]
pub struct DiagnosticInfo {
    pub path: PathBuf,
    pub eof_pos: SourceInfo,
}

impl DiagnosticInfo {
    pub fn new(path: PathBuf, eof_pos: SourceInfo) -> Self {
        Self { path, eof_pos }
    }
}

pub fn number_length(n: usize) -> usize {
    n.to_string().len()
}

#[derive(Debug, Default, Clone)]
pub struct DebugInfo {
    pub compiler_file: &'static str,
    pub compiler_line: u32,
    pub compiler_column: u32,
    pub internal_dump: String,
}

pub trait Dumpable {
    type Arena;
    fn dump(&self, arena: &Self::Arena) -> ColoredString;
}
