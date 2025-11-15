#![allow(unused)]

mod hfs {
    pub mod ast;
    pub mod builder;
    pub mod interpreter;
    pub mod lexer;
    pub mod parser;
    pub mod scope_stack;
    pub mod stack_analyzer;
    pub mod cfg_builder;
    pub mod hfs_mir;
    pub mod token;
    pub mod types;
    pub mod unresolved_ast;
    pub use ast::*;
    pub use builder::*;
    pub use interpreter::*;
    pub use lexer::*;
    pub use parser::*;
    pub use scope_stack::*;
    pub use stack_analyzer::*;
    pub use cfg_builder::*;
    pub use hfs_mir::*;
    pub use token::*;
    pub use types::*;
    pub use unresolved_ast::*;
}
use clap::{arg, Parser};
use std::path::PathBuf;

#[derive(Parser, Debug, Clone)]
#[command(author, version, about)]
struct Args {
    source: PathBuf,
    #[arg(short, long, default_value = "./a.out")]
    output: PathBuf,
}

fn main() {
    let args = Args::parse();
    let file = hfs::File::new(&args.source);
    let file_name = file.path.to_str().unwrap().to_string();

    let tokens = hfs::Lexer::tokenize(&file);

    let (unresolved_top_level_nodes, unresolved_ast_arena) = hfs::Parser::parse_tokens(tokens);

    let (top_level_nodes, ast_arena, scope_stack) =
        hfs::StackAnalyzer::resolve(unresolved_top_level_nodes, unresolved_ast_arena, file_name);
    // interpreter doesnt even need the top level nodes lol
    hfs::Interpreter::interpret(&ast_arena, &scope_stack);
}
