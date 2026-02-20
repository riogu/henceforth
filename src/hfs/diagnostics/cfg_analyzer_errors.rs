use std::{fmt::Display, fs, path::PathBuf};

use colored::{ColoredString, Colorize};

use crate::hfs::{
    error::{number_length, CompileError, DebugInfo},
    stack_analyzer_errors::{StackAnalyzerError, StackAnalyzerErrorKind},
    AstArena, CfgAnalyzer, CfgPrintable, IrArena, SourceInfo, Type,
};

#[derive(Debug)]
pub enum CfgAnalyzerErrorKind {
    IncorrectTupleLength(usize, usize),
    IncorrectPointerCount(usize, usize),
    MismatchingStackDepths(usize, usize),
    MismatchingTypes(Type, Type),
    NoStatementsInGlobalScope,
    StackUnderflow,
    ExpectedItemOnStack,
    ExpectedNetZeroStackEffectIfStmt(usize),
    ExpectedNetZeroStackEffectWhileLoop(usize),
}

#[derive(Debug)]
pub struct CfgAnalyzerError {
    pub kind: CfgAnalyzerErrorKind,
    pub path: PathBuf,
    pub source_info: SourceInfo,
    pub debug_info: DebugInfo,
}

#[macro_export]
macro_rules! cfg_analyzer_error {
    ($kind:expr, $arena:expr, $ast:expr, $source_info:expr) => {
        Err(Box::new(CfgAnalyzerError {
            kind: $kind,
            path: $arena.diagnostic_info.path.clone(),
            source_info: CfgAnalyzerError::merge_source_info($source_info),
            debug_info: $crate::hfs::diagnostics::error::DebugInfo {
                compiler_file: file!(),
                compiler_line: line!(),
                compiler_column: column!(),
                internal_dump: CfgAnalyzerError::dump_ast_and_ir($ast, $arena),
            },
        }))
    };
}

impl CfgAnalyzerError {
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

    pub fn dump_ast_and_ir(ast_arena: Option<&AstArena>, ir_arena: &IrArena) -> String {
        let ast_repr = match ast_arena {
            Some(ast_arena) => StackAnalyzerError::dump_ast(ast_arena),
            None => String::new(),
        };
        let functions =
            ir_arena.functions.iter().map(|func| func.get_repr(ir_arena).to_string()).collect::<Vec<String>>().join("\n");
        let global_vars =
            ir_arena.global_vars.iter().map(|var| var.get_repr(ir_arena).to_string()).collect::<Vec<String>>().join("\n");
        format!("{}\n\n{}\n\n{}", ast_repr, functions, global_vars)
    }
}

impl CompileError for CfgAnalyzerError {
    fn message(&self) -> (String, String) {
        match self.kind {
            CfgAnalyzerErrorKind::MismatchingStackDepths(_, _) => todo!(),
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

impl Display for CfgAnalyzerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_code = match self.source_code() {
            Ok(source_code) => source_code,
            Err(e) => ColoredString::from(format!("<source unavailable: {}>", e)),
        };
        write!(f, "{}\n{}\n{}{}", self.header(), self.location(), source_code, self.debug_info())
    }
}
