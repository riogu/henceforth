use std::{path::PathBuf, vec};

use crate::tokens::Token;
struct File {
    contents: String,
    path: PathBuf,
}

impl File {
    pub fn new(path: PathBuf) -> File {
        File {
            contents: "foo".to_string(),
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
    pub fn tokenize(&self) -> Vec<Token> {
        vec![]
    }
}
