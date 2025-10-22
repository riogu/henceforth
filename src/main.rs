#![feature(inherent_associated_types)]

pub mod lexer;
pub mod tokens;
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
    drop(tokens);
    println!("{:?}", file);
}
