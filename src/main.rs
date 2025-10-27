#![allow(unused)]

mod hfs {
    pub mod ast;
    pub mod unresolved_ast;
    pub mod builder;
    pub mod lexer;
    pub mod parser;
    // pub mod interpreter;
    pub mod token;
    pub mod types;
    pub mod stack_analyzer;
    pub mod scope_stack;
    pub use stack_analyzer::*;
    pub use scope_stack::*;
    pub use ast::*;
    pub use unresolved_ast::*;
    pub use builder::*;
    pub use lexer::*;
    pub use parser::*;
    // pub use interpreter::*;
    pub use token::*;
    pub use types::*;
}

use clap::{Parser, arg};
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
    println!("{:?}", args);
    println!("{:?}", file);
    let tokens = hfs::Lexer::tokenize(&file);
    let (unresolved_top_level_nodes, unresolved_ast_arena) = hfs::Parser::parse_tokens(tokens);
    let (top_level_nodes, ast_arena) = hfs::StackAnalyzer::resolve(unresolved_top_level_nodes, unresolved_ast_arena, file.path.to_str().unwrap().to_string());
}
