#![allow(unused)]

pub mod ast_node;
pub mod lexer;
pub mod parser;
pub mod semantic_analysis;
pub mod token;
pub mod types;

use std::path::PathBuf;

use clap::{arg, Parser};
use lexer::Lexer;

use crate::lexer::File;

#[derive(Parser, Debug, Clone)]
#[command(author, version, about)]
struct Args {
    source: PathBuf,

    #[arg(short, long, default_value = "./a.out")]
    output: PathBuf,
}

fn main() {
    let args = Args::parse();
    let file = File::new(&args.source);
    println!("{:?}", args);
    println!("{:?}", file);

    let lexer = Lexer::new();
    let tokens = lexer.tokenize(&file);
    // parser::Parser::parse_tokens(tokens);
}
