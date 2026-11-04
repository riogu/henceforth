use std::{any::Any, path::PathBuf, rc::Rc};

use crate::{
    hfs::{
        self, AstArena, File, IrArena, IrLowerer, Lexer, O0, OptPipeline, Parser, Span, StackAnalyzer, StackEnvironment, StackModifier, Token, UnresolvedAstArena, error::{CompileError, DiagnosticInfo}, get_eof_span,
    }, tools::hfs_assert::stack_assist::StackAssistant,
};
pub fn merge_spans(spans: Vec<Span>) -> Span {
    match spans.first() {
        Some(span) => span.merge(*spans.last().unwrap()),
        None => Span::default(),
    }
}

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
impl Byproduct for IrArena {
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

#[derive(PartialEq, Clone, Copy)]
pub enum Phase {
    Lexer,
    Parser,
    StackAnalyzer,
    CfgAnalyzer,
    Optimizer,
    Interpreter,
    StackAssist,
}

pub fn run_until(
    filename: &str,
    phase: Phase,
    opts: Option<Box<dyn OptPipeline>>,
) -> Result<Rc<dyn Byproduct>, Box<dyn CompileError>> {
    let path = PathBuf::from(filename);

    let file = File::new(path);

    let tokens = Lexer::tokenize(&file)?;
    let diagnostic_info = Rc::new(DiagnosticInfo::new(file.path, get_eof_span(&tokens)));

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

    hfs::process_environments(ast_arena.stack_environments.clone(), diagnostic_info.clone())?;

    if phase == Phase::StackAssist {
        return Ok(Rc::new(()));
    }

    let (_, mut ir_arena) = IrLowerer::lower_to_mir(top_level_nodes, ast_arena, diagnostic_info.clone())?;

    if phase == Phase::CfgAnalyzer {
        return Ok(Rc::new(ir_arena));
    }

    if let Some(mut opts) = opts {
        OptPipeline::run_iteratively(&mut *opts, &mut ir_arena);
    } else {
        OptPipeline::run_iteratively(&mut O0::new(), &mut ir_arena);
    }

    if phase == Phase::Optimizer {
        return Ok(Rc::new(ir_arena));
    }

    Ok(Rc::new(()))
}

pub fn process_environments(environments: Vec<StackEnvironment>, diagnostic_info: Rc<DiagnosticInfo>) -> Result<(), Box<dyn CompileError>> {
    for env in environments {
        if let Some(_) = env.modifiers.iter().find(|m| matches!(m, StackModifier::Pre(_, _)) || matches!(m, StackModifier::Post(_, _)))
        {
            let assistant = StackAssistant::new(diagnostic_info.clone());
            assistant.find_error_and_correction(env)?;
        }
    }
    Ok(())
}
