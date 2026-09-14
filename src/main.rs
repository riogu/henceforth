#![allow(unused)]

use std::{error::Error, path::PathBuf, process::exit, rc::Rc};

use clap::{Parser, arg};
use henceforth::hfs::{
    self, OptPipeline,
    error::{CompileError, DiagnosticInfo},
    get_eof_span,
    ir_lowerer_errors::IrLowererError,
};

#[derive(Parser, Debug, Clone)]
#[command(author, version, about)]
struct Args {
    source: PathBuf,
    #[arg(short, long, default_value = "./a.out")]
    output: PathBuf,
    #[arg(long, value_enum, default_value = "interpret")]
    backend: hfs::BackendKind,
    #[arg(long)]
    print_ir_pre_opt: bool,
    #[arg(long)]
    print_ir_post_opt: bool,
}

fn run() -> Result<i32, Box<dyn CompileError>> {
    let args = Args::parse();
    let file = hfs::File::new(args.source);
    let file_name = file.path.to_str().unwrap().to_string();

    let tokens = hfs::Lexer::tokenize(&file)?;
    let diagnostic_info = Rc::new(DiagnosticInfo::new(file.path, get_eof_span(&tokens)));

    let (unresolved_top_level_nodes, unresolved_ast_arena) = hfs::Parser::parse_tokens(tokens.clone(), diagnostic_info.clone())?;

    let (top_level_nodes, ast_arena, scope_stack) =
        hfs::StackAnalyzer::resolve(unresolved_top_level_nodes, unresolved_ast_arena.clone(), diagnostic_info.clone())?;

    let (top_level_insts, mut ir_arena) =
        hfs::IrLowerer::lower_to_mir(top_level_nodes, ast_arena.clone(), diagnostic_info.clone())?;

    if args.print_ir_pre_opt {
        println!("IR before optimizations:{}", IrLowererError::dump_ast_and_ir(None, &ir_arena));
    }

    hfs::OptPipeline::run_iteratively(&mut hfs::O0::new(), &mut ir_arena);

    if args.print_ir_post_opt {
        println!("IR after optimizations:{}", IrLowererError::dump_ast_and_ir(None, &ir_arena));
    }

    Ok(hfs::backend::run(args.backend, ir_arena, top_level_insts, scope_stack, Some(args.output)))
}

fn main() {
    match run() {
        Ok(exit_code) => exit(exit_code),
        Err(e) => {
            eprintln!("{}", e);
            exit(1);
        },
    }
}
