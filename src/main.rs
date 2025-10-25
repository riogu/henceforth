#![allow(unused)]

mod hfs {
    pub mod ast_node;
    pub mod builder;
    pub mod lexer;
    pub mod parser;
    pub mod semantic_analysis;
    pub mod token;
    pub use lexer::*;
    pub use parser::*;
    pub use ast_node::*;
    pub use builder::*;
    pub use semantic_analysis::*;
    pub use token::*;
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
    let (top_level_nodes, ast_arena) = hfs::Parser::parse_tokens(tokens);
    hfs::Analyzer::analyze(&top_level_nodes, file.path.to_str().unwrap().to_string(), ast_arena);
}
