use crate::tokens::Token;
use std::{fs, path::PathBuf, vec};

struct File {
    contents: Vec<String>,
    path: PathBuf,
}

impl File {
    pub fn new(path: PathBuf) -> File {
        File {
            contents: fs::read_to_string(&path)
                .expect("Could not read file.")
                .lines()
                .map(String::from)
                .collect(),
            path,
        }
    }
}

pub struct Lexer {
    fumo_line: String,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            fumo_line: String::new(),
        }
    }
    #[must_use]
    pub fn tokenize(&self, file: File) -> Vec<Token> {
        let mut char_iter = file.contents.chars().peekable();

        while let Some(char) = char_iter.next() {
            match char {
                // Whitespace
                ' ' | '\t' | '\r' => todo!(),
                '\n' => todo!(),
                // Single-character delimiters
                '(' => todo!(),
                ')' => todo!(),
                '{' => todo!(),
                '}' => todo!(),
                '[' => todo!(),
                ']' => todo!(),
                ';' => todo!(),
                ',' => todo!(),
                '.' => todo!(),
                ':' => todo!(),
                '%' => todo!(),
                // Operators that may be compound
                '+' => todo!(), // + or +=
                '-' => todo!(), // - or -= or ->
                '*' => todo!(), // * or *=
                '/' => todo!(), // / or /= or // (comments)
                '=' => todo!(), // = or ==
                '!' => todo!(), // ! or !=
                '<' => todo!(), // < or <=
                '>' => todo!(), // > or >=
                '&' => todo!(), // &&
                '|' => todo!(), // ||

                '"' => todo!(),
                '0'..='9' => todo!(),

                'a'..='z' | 'A'..='Z' | '_' => todo!(),
                _ => todo!(),
            }
        }
        vec![]
    }
}
