use std::{any::Any, path::PathBuf, rc::Rc};

use crate::hfs::{
    AstArena, IrLowerer, File, IrArena, Lexer, O0, OptPipeline, Parser, StackAnalyzer, Token, UnresolvedAstArena,
    error::{CompileError, DiagnosticInfo},
    get_eof_source_info,
};

pub trait Byproduct {
    fn as_any(&self) -> &dyn Any;
}

impl Byproduct for UnresolvedAstArena {
    fn as_any(&self) -> &dyn Any { self }
}
impl Byproduct for AstArena {
    fn as_any(&self) -> &dyn Any { self }
}
impl Byproduct for IrArena {
    fn as_any(&self) -> &dyn Any { self }
}
impl Byproduct for Vec<Token> {
    fn as_any(&self) -> &dyn Any { self }
}
impl Byproduct for () {
    fn as_any(&self) -> &dyn Any { self }
}

#[derive(PartialEq, Clone, Copy)]
pub enum Phase {
    Lexer,
    Parser,
    StackAnalyzer,
    CfgAnalyzer,
    Optimizer,
    Interpreter,
}

pub fn run_until(filename: &str, phase: Phase) -> Result<Rc<dyn Byproduct>, Box<dyn CompileError>> {
    let path = PathBuf::from(filename);

    let file = File::new(path);

    let tokens = Lexer::tokenize(&file)?;
    let diagnostic_info = Rc::new(DiagnosticInfo::new(file.path, get_eof_source_info(&tokens)));

    if phase == Phase::Lexer {
        return Ok(Rc::new(tokens));
    }

    let (unresolved_top_level_nodes, unresolved_ast_arena) = Parser::parse_tokens(tokens, diagnostic_info.clone())?;

    if phase == Phase::Parser {
        return Ok(Rc::new(unresolved_ast_arena));
    }

    let (top_level_nodes, ast_arena, _) =
        StackAnalyzer::resolve(unresolved_top_level_nodes, unresolved_ast_arena, diagnostic_info.clone())?;

    if phase == Phase::StackAnalyzer {
        return Ok(Rc::new(ast_arena));
    }

    let (_, mut ir_arena) = IrLowerer::lower_to_mir(top_level_nodes, ast_arena, diagnostic_info.clone())?;

    if phase == Phase::CfgAnalyzer {
        return Ok(Rc::new(ir_arena));
    }

    OptPipeline::run_iteratively(&mut O0::new(), &mut ir_arena);

    if phase == Phase::Optimizer {
        return Ok(Rc::new(ir_arena));
    }

    Ok(Rc::new(())) // Interpreter case
}
