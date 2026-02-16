#![allow(unused)]

mod hfs {
    pub mod ast;
    // pub mod ast_interpreter;
    pub mod builder;
    pub mod cfg_analyzer;
    pub mod diagnostics;
    pub mod hfs_mir;
    pub mod interpreter;
    pub mod lexer;
    pub mod parser;
    pub mod scope_stack;
    pub mod stack_analyzer;
    pub mod token;
    pub mod types;
    pub mod unresolved_ast;
    pub use ast::*;
    pub use builder::*;
    pub use cfg_analyzer::*;
    pub use diagnostics::*;
    pub use hfs_mir::*;
    pub use interpreter::*;
    pub use lexer::*;
    pub use parser::*;
    pub use scope_stack::*;
    pub use stack_analyzer::*;
    pub use token::*;
    pub use types::*;
    pub use unresolved_ast::*;
}
use std::{error::Error, path::PathBuf, process::exit};

use clap::{Parser, arg};

use crate::hfs::error::{CompileError, DiagnosticInfo};

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

    let (unresolved_top_level_nodes, unresolved_ast_arena) = hfs::Parser::parse_tokens(tokens, file.path)?;

    let (top_level_nodes, ast_arena, scope_stack) =
        hfs::StackAnalyzer::resolve(unresolved_top_level_nodes, unresolved_ast_arena, file_name);

    let (top_level_insts, ir_arena) = hfs::CfgAnalyzer::lower_to_mir(top_level_nodes, ast_arena);
    ir_arena.dump(&top_level_insts);
    hfs::Interpreter::interpret(ir_arena, top_level_insts, scope_stack);

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
        exit(1);
    }
}
