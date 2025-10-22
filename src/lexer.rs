use crate::tokens::SourceInfo;
use crate::tokens::Token;
use std::{fs, path::PathBuf};

#[derive(Debug)]
pub struct File<'a> {
    contents: Vec<String>,
    path: &'a PathBuf,
}

impl<'a> File<'a> {
    pub fn new(path: &'a PathBuf) -> File<'a> {
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
    pub fn tokenize<'a>(&self, file: &'a File) -> Vec<Token<'a>> {
        let mut line_iter = file.contents.iter();
        let mut tokens = Vec::<Token>::new();
        let mut line_offset = 0;

        for (line_number, line_string) in file.contents.iter().enumerate() {
            let mut chars_iter = line_string.chars().peekable();
            while let Some(char) = chars_iter.next() {
                let kind = match char {
                    // Whitespace
                    ' ' | '\t' | '\r' => todo!(),
                    // Single-character delimiters
                    '(' => Token::Kind::LeftParen,
                    ')' => Token::Kind::RightParen,
                    '{' => Token::Kind::LeftBrace,
                    '}' => Token::Kind::RightBrace,
                    '[' => Token::Kind::LeftBracket,
                    ']' => Token::Kind::RightBracket,
                    ';' => Token::Kind::Semicolon,
                    ',' => Token::Kind::Comma,
                    '.' => Token::Kind::Dot,
                    ':' => Token::Kind::Colon,
                    '%' => Token::Kind::Percent,
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
                };
                line_offset += 1;
                tokens.push(Token::new(
                    kind,
                    SourceInfo::new(line_number, line_offset, 1, line_string),
                ));
            }
        }
        tokens
    }
}
