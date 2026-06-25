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

    fn get_path(&self) -> PathBuf { self.path.clone() }
    fn get_debug_info(&self) -> DebugInfo { self.debug_info.clone() }
    fn get_span(&self) -> Span { self.span }
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
