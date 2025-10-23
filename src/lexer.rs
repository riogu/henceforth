use crate::token::Literal;
use crate::token::SourceInfo;
use crate::token::Token;
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
                    ' ' | '\t' | '\r' => todo!(),
                    '(' => Token::Kind::LeftParen,
                    ')' => Token::Kind::RightParen,
                    '{' => Token::Kind::LeftBrace,
                    '}' => Token::Kind::RightBrace,
                    ';' => Token::Kind::Semicolon,
                    '%' => Token::Kind::Percent,
                    '@' => Token::Kind::At,
                    '+' => Token::Kind::Plus,
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
                    }
                    '*' => Token::Kind::Star,
                    '/' => {
                        if let Some(_) = chars_iter.next_if_eq(&'/') {
                            continue;
                        } else {
                            Token::Kind::Slash
                        }
                    }
                    '=' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            Token::Kind::Equal
                        } else {
                            panic!("lexer error")
                        }
                    } // ==
                    '!' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            Token::Kind::NotEqual
                        } else {
                            Token::Kind::Not
                        }
                    } // ! or !=
                    '<' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            Token::Kind::LessEqual
                        } else {
                            Token::Kind::Less
                        }
                    } // < or <=
                    '>' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            Token::Kind::GreaterEqual
                        } else {
                            Token::Kind::Greater
                        }
                    } // > or >=
                    '&' => {
                        if let Some(_) = chars_iter.next_if_eq(&'&') {
                            Token::Kind::And
                        } else {
                            if let Some(_) = chars_iter.next_if_eq(&'=') {
                                Token::Kind::MoveAssign
                            } else {
                                panic!("lexer error")
                            }
                        } // &&
                    }
                    '|' => {
                        if let Some(_) = chars_iter.next_if_eq(&'|') {
                            Token::Kind::Or
                        } else {
                            panic!("lexer error")
                        }
                    } // ||

                    '"' => {
                        let mut lit = String::new();
                        loop {
                            if let Some(_) = chars_iter.next_if_eq(&'"') {
                                break;
                            } else {
                                if let Some(c) = chars_iter.next() {
                                    lit.push(c);
                                } else {
                                    panic!("lexer error");
                                }
                            }
                        }
                        Token::Kind::Literal(Literal::String(lit))
                    }
                    '0'..='9' => {
                        let mut lit = String::new();
                        while let Some(c) =
                            chars_iter.next_if(|c| ('0'..='9').contains(c) || c == &'.')
                        {
                            lit.push(c);
                        }
                        let number: Result<i32, f32> = match lit.parse::<i32>() {
                            Ok(number) => Ok(number),
                            Err(_) => match lit.parse::<f32>() {
                                Ok(number) => Err(number),
                                Err(_) => panic!("lexer error"),
                            },
                        };
                        match number {
                            Ok(int) => Token::Kind::Literal(Literal::Integer(int)),
                            Err(float) => Token::Kind::Literal(Literal::Float(float)),
                        }
                    }
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let mut lit = String::new();
                        while let Some(c) = chars_iter.next_if(|c| {
                            ('a'..='z').contains(c) || ('A'..='Z').contains(c) || c == &'_'
                        }) {
                            lit.push(c);
                        }
                        match lit.as_str() {
                            "let" => Token::Kind::Let,
                            "if" => Token::Kind::If,
                            "else" => Token::Kind::Else,
                            "elif" => Token::Kind::Elif,
                            "while" => Token::Kind::While,
                            "break" => Token::Kind::Break,
                            "continue" => Token::Kind::Continue,
                            "return" => Token::Kind::Return,
                            "fn" => Token::Kind::Fn,

                            "i32" => Token::Kind::IntT,
                            "string" => Token::Kind::StringT,
                            "f32" => Token::Kind::FloatT,
                            "bool" => Token::Kind::BoolT,

                            _ => Token::Kind::Literal(Literal::Identifier(lit)),
                        }
                    }
                    _ => panic!("lexer error"),
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
