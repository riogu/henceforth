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
                    ';' => Token::Kind::Semicolon,
                    '%' => Token::Kind::Percent,
                    '@' => Token::Kind::At,
                    '+' => Token::Kind::Plus,

                    // Operators that may be compound
                    ':' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            Token::Kind::CopyAssign
                        } else {
                            Token::Kind::Colon
                        }
                    }
                    '-' => {
                        if let Some(_) = chars_iter.next_if_eq(&'>') {
                            Token::Kind::Arrow
                        } else {
                            Token::Kind::Minus
                        }
                    } // - or ->
                    '*' => Token::Kind::Star, // *
                    '/' => {
                        if let Some(_) = chars_iter.next_if_eq(&'/') {
                            continue;
                        } else {
                            Token::Kind::Slash
                        }
                    } // / or /= or // (comments)
                    '=' => todo!(),           // = or ==
                    '!' => todo!(),           // ! or !=
                    '<' => todo!(),           // < or <=
                    '>' => todo!(),           // > or >=
                    '&' => todo!(),           // &&
                    '|' => todo!(),           // ||

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
