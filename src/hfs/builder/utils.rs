use std::{any::Any, path::PathBuf, rc::Rc};

use crate::hfs::{AstArena, CfgAnalyzer, File, InstArena, Lexer, Parser, StackAnalyzer, Token, UnresolvedAstArena};

pub trait Byproduct {
    fn as_any(&self) -> &dyn Any;
}

impl Byproduct for UnresolvedAstArena {
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Byproduct for AstArena {
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Byproduct for InstArena {
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Byproduct for Vec<Token> {
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Byproduct for () {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(PartialEq)]
pub enum Phase {
    Lexer,
    Parser,
    StackAnalyzer,
    CfgAnalyzer,
    Interpreter,
}

pub fn run_until(filename: &str, phase: Phase) -> Rc<dyn Byproduct> {
    let path = PathBuf::from(filename);

    let file = File::new(&path);
    let file_name = file.path.to_str().unwrap().to_string();

    let tokens = Lexer::tokenize(&file);

    if phase == Phase::Lexer {
        return Rc::new(tokens);
    }

    let (unresolved_top_level_nodes, unresolved_ast_arena) = Parser::parse_tokens(tokens);

    if phase == Phase::Parser {
        return Rc::new(unresolved_ast_arena);
    }

    let (top_level_nodes, ast_arena, scope_stack) =
        StackAnalyzer::resolve(unresolved_top_level_nodes, unresolved_ast_arena, file_name);

    if phase == Phase::StackAnalyzer {
        return Rc::new(ast_arena);
    }

    let inst_arena = CfgAnalyzer::lower_to_mir(top_level_nodes, ast_arena);

    if phase == Phase::CfgAnalyzer {
        return Rc::new(inst_arena);
    }

    return Rc::new(());
}
