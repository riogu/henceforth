#![feature(inherent_associated_types)]
#![allow(unused)]
pub mod lexer;
pub mod token;
pub mod parser;
pub mod ast_node;
pub mod semantic_analysis;

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
    let lexer = Lexer::new();
    let tokens = lexer.tokenize(&file);
    println!("{:?}", file);
}
