use std::{fmt::Display, fs, path::PathBuf};

use colored::{ColoredString, Colorize};

use crate::hfs::{
    error::{number_length, CompileError, DebugInfo},
    AstArena, SourceInfo, Type,
};

#[derive(Debug)]
pub enum JumpKeyword {
    Break,
    Continue,
    Return,
}

impl Display for JumpKeyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JumpKeyword::Break => write!(f, "break"),
            JumpKeyword::Continue => write!(f, "continue"),
            JumpKeyword::Return => write!(f, "return"),
        }
    }
}

#[derive(Debug)]
pub enum StackAnalyzerErrorKind {
    StackUnderflow,
    ExpectedItemOnStack,
    IncorrectNumberReturnValues(usize, usize),
    TypeMismatchReturnValues(Type, Type),
    TypeMismatch(Type, Type),
    IncorrectTupleLength(usize, usize),
    IncorrectPointerCount(usize, usize),
    MismatchingStackDepths(usize, usize),
    ExpectedNetZeroStackEffectIfStmt(usize),
    ExpectedNetZeroStackEffectWhileLoop(usize),
    FoundXOutsideWhileLoop(JumpKeyword),
    AssignValueToFunction,
    TooManyDereferences(usize, usize),
    CallVariableAsFunction,
    UseOfUndeclaredIdentifier(String),
}

#[derive(Debug)]
pub struct StackAnalyzerError {
    pub kind: StackAnalyzerErrorKind,
    pub path: PathBuf,
    pub source_info: SourceInfo,
    pub debug_info: DebugInfo,
}

#[macro_export]
macro_rules! stack_analyzer_error {
    ($kind:expr, $arena:expr, $source_info:expr) => {
        Err(Box::new(StackAnalyzerError {
            kind: $kind,
            path: $arena.diagnostic_info.path.clone(),
            source_info: StackAnalyzerError::merge_source_info($source_info),
            debug_info: $crate::hfs::diagnostics::error::DebugInfo {
                compiler_file: file!(),
                compiler_line: line!(),
                compiler_column: column!(),
                internal_dump: StackAnalyzerError::dump_ast($arena),
            },
        }))
    };
}
impl StackAnalyzerError {
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

    pub fn dump_ast(arena: &AstArena) -> String {
        format!("")
    }
}

impl CompileError for StackAnalyzerError {
    fn message(&self) -> (String, String) {
        match &self.kind {
            StackAnalyzerErrorKind::StackUnderflow =>
                (String::from("stack underflow"), String::from("stack underflow occurred here")),
            StackAnalyzerErrorKind::ExpectedItemOnStack => (String::from("expected item on stack"), String::new()),
            StackAnalyzerErrorKind::IncorrectNumberReturnValues(expected, actual) =>
                (format!("expected {} values on stack for return, found {}", expected, actual), String::new()),
            StackAnalyzerErrorKind::TypeMismatchReturnValues(expected, actual) =>
                (format!("expected {} on stack for return, found {}", expected, actual), String::new()),
            StackAnalyzerErrorKind::TypeMismatch(expected, actual) =>
                (format!("expected {}, found {}", expected, actual), format!("found {}", actual)),
            StackAnalyzerErrorKind::IncorrectTupleLength(expected, actual) =>
                (format!("expected a tuple of size {}, found a tuple of size {}", expected, actual), String::new()),
            StackAnalyzerErrorKind::IncorrectPointerCount(expected, actual) =>
                (format!("expected a pointer count of {}, found a pointer count of {}", expected, actual), String::new()),
            StackAnalyzerErrorKind::MismatchingStackDepths(expected, actual) =>
                (format!("expected a stack depth of {}, found a stack depth of {}", expected, actual), String::new()),
            StackAnalyzerErrorKind::ExpectedNetZeroStackEffectIfStmt(found) => (
                format!("expected a net-zero stack effect on all branches, found a stack depth difference of {}", found),
                String::new(),
            ),
            StackAnalyzerErrorKind::ExpectedNetZeroStackEffectWhileLoop(found) => (
                format!("expected while loop to maintain a net-zero stack effect, found a stack depth difference of {}", found),
                String::new(),
            ),
            StackAnalyzerErrorKind::FoundXOutsideWhileLoop(jump_keyword) =>
                (format!("found {} outside while loop", jump_keyword), format!("found {}", jump_keyword)),
            StackAnalyzerErrorKind::AssignValueToFunction => (format!("cannot assign value to a function"), String::new()),
            StackAnalyzerErrorKind::TooManyDereferences(actual, max) =>
                (format!("cannot dereference {} times", actual), format!("type only has {} levels of indirection", max)),
            StackAnalyzerErrorKind::CallVariableAsFunction => (format!("cannot call variable as a function"), String::new()),
            StackAnalyzerErrorKind::UseOfUndeclaredIdentifier(name) =>
                (format!("use of undeclared identifier '{}'", name), format!("undeclared identifier")),
        }
    }

    fn header(&self) -> colored::ColoredString {
        format!("{} {}", "error:".red().bold(), self.message().0.bold()).into()
    }

    fn location(&self) -> colored::ColoredString {
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

    fn source_code(&self) -> Result<colored::ColoredString, Box<dyn std::error::Error>> {
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

impl Display for StackAnalyzerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_code = match self.source_code() {
            Ok(source_code) => source_code,
            Err(e) => ColoredString::from(format!("<source unavailable: {}>", e)),
        };
        write!(f, "{}\n{}\n{}{}", self.header(), self.location(), source_code, self.debug_info())
    }
}
