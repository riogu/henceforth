use std::{fmt::Display, path::PathBuf};

use colored::ColoredString;

use crate::{
    hfs::{
        AstArena, IrArena, IrFuncId, Span,
        error::{CompileError, DebugInfo, Label, Snippet},
        prettify_ir, print,
        stack_analyzer_errors::StackAnalyzerError,
    },
    impl_display_for_error,
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
    ExpectedNetZeroStackEffectWhileLoop(i64),
}

#[derive(Debug)]
pub struct IrLowererError {
    pub kind: IrLowererErrorKind,
    pub path: PathBuf,
    pub span: Span,
    pub debug_info: DebugInfo,
    pub current_function: Option<String>,
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
            current_function: $arena.diagnostic_info.current_function.borrow().clone(),
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
            Some(output) if ast_repr.is_empty() => prettify_ir(output),
            Some(output) => format!("{}\n\n{}", ast_repr, prettify_ir(output)),
            None => format!("{}", ast_repr),
        }
        // let mut glob_vars: Vec<_> = ir_arena.global_vars.iter().collect();
        // glob_vars.sort_by_key(|(k, _)| *k);
        // let global_vars = glob_vars.into_iter().map(|(_, var)| var.get_repr(ir_arena).to_string()).collect::<Vec<_>>().join("\n");
    }
}

impl CompileError for IrLowererError {
    fn message(&self) -> String {
        match &self.kind {
            IrLowererErrorKind::StackUnderflow => String::from("stack underflow"),
            IrLowererErrorKind::ExpectedItemOnStack => String::from("expected item on stack"),
            IrLowererErrorKind::ExpectedNetZeroStackEffectIfStmt(found) => {
                format!("expected a net-zero stack effect on all branches, found a stack depth difference of {}", found)
            },
            IrLowererErrorKind::ExpectedNetZeroStackEffectWhileLoop(found) => {
                format!("expected while loop to maintain a net-zero stack effect, found a stack depth difference of {}", found)
            },
            IrLowererErrorKind::MismatchingStackDepths(expected, actual) => {
                format!("expected a stack depth of {}, found a stack depth of {}", expected, actual)
            },
            IrLowererErrorKind::IncorrectTupleLength(expected, actual) => {
                format!("expected a tuple of size {}, found a tuple of size {}", expected, actual)
            },
            IrLowererErrorKind::IncorrectPointerCount(expected, actual) => {
                format!("expected a pointer count of {}, found a pointer count of {}", expected, actual)
            },
            IrLowererErrorKind::MismatchingTypes(expected, actual) => {
                format!("expected {}, found {}", expected, actual)
            },
            IrLowererErrorKind::NoStatementsInGlobalScope => String::from("statements are not allowed in the global scope"),
        }
    }

    fn get_debug_info(&self) -> DebugInfo {
        self.debug_info.clone()
    }
    fn current_function(&self) -> Option<String> {
        self.current_function.clone()
    }

    fn snippets(&self) -> Vec<Snippet> {
        {
            vec![Snippet {
                path: self.path.clone(),
                labels: vec![Label {
                    span: self.span,
                    message: match &self.kind {
                        IrLowererErrorKind::StackUnderflow => Some("stack underflow occured here".into()),
                        IrLowererErrorKind::MismatchingTypes(_, actual) => Some(format!("found {}", actual)),
                        _ => None,
                    },
                }],
            }]
        }
    }
    fn get_span(&self) -> Span {
        self.span
    }
}

impl_display_for_error!(IrLowererError);
