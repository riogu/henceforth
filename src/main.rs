#![allow(unused)]

use std::{error::Error, path::PathBuf, process::exit, rc::Rc};

use clap::{arg, Parser};
use henceforth::hfs::{
    self,
    cfg_analyzer_errors::CfgAnalyzerError,
    error::{CompileError, DiagnosticInfo},
    get_eof_source_info, OptPipeline,
};

#[derive(Parser, Debug, Clone)]
#[command(author, version, about)]
struct Args {
    source: PathBuf,
    #[arg(short, long, default_value = "./a.out")]
    output: PathBuf,
}

fn run() -> Result<(), Box<dyn CompileError>> {
    let args = Args::parse();
    let file = hfs::File::new(args.source);
    let file_name = file.path.to_str().unwrap().to_string();

    let tokens = hfs::Lexer::tokenize(&file)?;
    let diagnostic_info = Rc::new(DiagnosticInfo::new(file.path, get_eof_source_info(&tokens)));

    let (unresolved_top_level_nodes, unresolved_ast_arena) = hfs::Parser::parse_tokens(tokens.clone(), diagnostic_info.clone())?;

    let (top_level_nodes, ast_arena, scope_stack) =
        hfs::StackAnalyzer::resolve(unresolved_top_level_nodes, unresolved_ast_arena.clone(), diagnostic_info.clone())?;

    let (top_level_insts, mut ir_arena) =
        hfs::CfgAnalyzer::lower_to_mir(top_level_nodes, ast_arena.clone(), diagnostic_info.clone())?;

    ir_arena.dump(&top_level_insts);
    // println!("\nIR before optimizations:{}", CfgAnalyzerError::dump_ast_and_ir(None, &ir_arena));
    hfs::OptPipeline::run_iteratively(&mut hfs::O0::new(), &mut ir_arena);
    // println!("IR after optimizations:{}", CfgAnalyzerError::dump_ast_and_ir(None, &ir_arena));

    hfs::Interpreter::interpret(ir_arena, top_level_insts, scope_stack);

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
        exit(1);
    }
}
