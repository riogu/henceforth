use std::{fmt::Display, fs, path::PathBuf};

use colored::{ColoredString, Colorize};

use crate::hfs::{
    AstArena, IrArena, IrFuncId, Span,
    error::{CompileError, DebugInfo, number_length},
    prettify_ir, print,
    stack_analyzer_errors::StackAnalyzerError,
};

#[derive(Debug)]
pub enum IrLowererErrorKind {
    IncorrectTupleLength(usize, usize),
    IncorrectPointerCount(usize, usize),
    MismatchingStackDepths(usize, usize),
    MismatchingTypes(String, String),
    NoStatementsInGlobalScope,
    StackUnderflow,
    ExpectedItemOnStack,
    ExpectedNetZeroStackEffectIfStmt(usize),
    ExpectedNetZeroStackEffectWhileLoop(usize),
}

#[derive(Debug)]
pub struct IrLowererError {
    pub kind: IrLowererErrorKind,
    pub path: PathBuf,
    pub span: Span,
    pub debug_info: DebugInfo,
}

#[macro_export]
macro_rules! ir_lowerer_error {
    ($kind:expr, $arena:expr, $ast:expr, $span:expr) => {
        Err(Box::new($crate::hfs::diagnostics::ir_lowerer_errors::IrLowererError {
            kind: $kind,
            path: $arena.diagnostic_info.path.clone(),
            span: $span,
            debug_info: $crate::hfs::diagnostics::error::DebugInfo {
                compiler_file: file!(),
                compiler_line: line!(),
                compiler_column: column!(),
                internal_dump: $crate::hfs::diagnostics::ir_lowerer_errors::IrLowererError::dump_ast_and_ir($ast, $arena),
            },
        }))
    };
}

impl IrLowererError {
    pub fn dump_ast_and_ir(ast_arena: Option<&AstArena>, ir_arena: &IrArena) -> String {
        let ast_repr = match ast_arena {
            Some(ast_arena) => StackAnalyzerError::dump_ast(ast_arena),
            None => String::new(),
        };

        let mut functions: Vec<_> = ir_arena.functions.iter().collect();
        functions.sort_by_key(|(k, _)| *k);
        let func_ids: Vec<IrFuncId> = ir_arena.functions.keys().collect();
        let output = print(&func_ids, &ir_arena);
        match output {
            Some(output) => format!("{}\n\n{}", ast_repr, prettify_ir(output)),
            None => format!("{}", ast_repr),
        }
        // let mut glob_vars: Vec<_> = ir_arena.global_vars.iter().collect();
        // glob_vars.sort_by_key(|(k, _)| *k);
        // let global_vars = glob_vars.into_iter().map(|(_, var)| var.get_repr(ir_arena).to_string()).collect::<Vec<_>>().join("\n");
    }
}

impl CompileError for IrLowererError {
    // TODO: fix multi-line spans
    fn get_line(&self) -> usize { return self.span.start.line; }
    fn message(&self) -> (String, String) {
        match &self.kind {
            IrLowererErrorKind::StackUnderflow =>
                (String::from("stack underflow"), String::from("stack underflow occurred here")),
            IrLowererErrorKind::ExpectedItemOnStack => (String::from("expected item on stack"), String::new()),
            IrLowererErrorKind::ExpectedNetZeroStackEffectIfStmt(found) => (
                format!("expected a net-zero stack effect on all branches, found a stack depth difference of {}", found),
                String::new(),
            ),
            IrLowererErrorKind::ExpectedNetZeroStackEffectWhileLoop(found) => (
                format!("expected while loop to maintain a net-zero stack effect, found a stack depth difference of {}", found),
                String::new(),
            ),
            IrLowererErrorKind::MismatchingStackDepths(expected, actual) =>
                (format!("expected a stack depth of {}, found a stack depth of {}", expected, actual), String::new()),
            IrLowererErrorKind::IncorrectTupleLength(expected, actual) =>
                (format!("expected a tuple of size {}, found a tuple of size {}", expected, actual), String::new()),
            IrLowererErrorKind::IncorrectPointerCount(expected, actual) =>
                (format!("expected a pointer count of {}, found a pointer count of {}", expected, actual), String::new()),
            IrLowererErrorKind::MismatchingTypes(expected, actual) =>
                (format!("expected {}, found {}", expected, actual), format!("found {}", actual)),
            IrLowererErrorKind::NoStatementsInGlobalScope =>
                (String::from("statements are not allowed in the global scope"), String::new()),
        }
    }

    fn header(&self) -> colored::ColoredString { format!("{} {}", "error:".red().bold(), self.message().0.bold()).into() }

    // TODO: fix multi-line spans
    fn location(&self) -> colored::ColoredString {
        format!(
            "{}{} {}:{}:{}",
            " ".repeat(number_length(self.span.start.line)),
            "-->".blue(),
            self.path.to_str().unwrap(),
            self.span.start.line,
            self.span.start.col
        )
        .into()
    }

    // TODO: fix multi-line spans
    fn source_code(&self) -> Result<colored::ColoredString, Box<dyn std::error::Error>> {
        let source = fs::read_to_string(&self.path).map_err(|e| format!("Could not read source file: {}", e))?;

        let line = source
            .lines()
            .nth(self.span.start.line - 1)
            .ok_or_else(|| format!("Line {} not found in file", self.span.start.line))?
            .replace("\t", "    ");

        let mut error_pointer = " ".repeat(self.span.start.col - 1);
        error_pointer.push_str(format!("{} {}", "^".repeat(self.span.end.col - self.span.start.col), self.message().1).as_str());
        return Ok(ColoredString::from(format!(
            "{} {}\n{} {} {}\n{} {} {}",
            " ".repeat(number_length(self.span.start.line)),
            "|".blue().bold(),
            self.span.start.line.to_string().blue().bold(),
            "|".blue().bold(),
            line,
            " ".repeat(number_length(self.span.start.line)),
            "|".blue().bold(),
            error_pointer.red().bold()
        )));
    }

    fn debug_info(&self) -> colored::ColoredString {
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

impl Display for IrLowererError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_code = match self.source_code() {
            Ok(source_code) => source_code,
            Err(e) => ColoredString::from(format!("<source unavailable: {}>", e)),
        };
        write!(f, "{}\n{}\n{}{}", self.header(), self.location(), source_code, self.debug_info())
    }
}
