use std::{fs, path::PathBuf};

use crate::{
    hfs::{
        error::CompileError,
        lexer_errors::{LexerError, LexerErrorKind},
        token::{Literal, SourceInfo, Token, TokenKind},
        VALID_STACK_KEYWORDS,
    },
    lexer_error,
};

#[derive(Debug)]
pub struct File {
    pub contents: Vec<String>,
    pub path: PathBuf,
}

impl File {
    pub fn new(path: PathBuf) -> File {
        File { contents: fs::read_to_string(&path).expect("Could not read file.").lines().map(String::from).collect(), path }
    }
}

pub struct Lexer {} // idk how to make this a namespace

impl Lexer {
    #[must_use]
    pub fn tokenize<'a>(file: &'a File) -> Result<Vec<Token>, Box<dyn CompileError>> {
        let mut tokens = Vec::<Token>::new();
        for (line_number, line_string) in file.contents.iter().enumerate() {
            let mut line_offset = 1;
            let mut chars_iter = line_string.chars().peekable();
            while let Some(char) = chars_iter.next() {
                let kind = match char {
                    ' ' | '\r' => {
                        line_offset += 1;
                        continue;
                    },
                    '\t' => {
                        line_offset += 4;
                        continue;
                    },
                    '(' => TokenKind::LeftParen,
                    ')' => TokenKind::RightParen,
                    '{' => TokenKind::LeftBrace,
                    '}' => TokenKind::RightBrace,
                    ';' => TokenKind::Semicolon,
                    '%' => TokenKind::Percent,
                    '@' => {
                        let mut lit = char.to_string();
                        while let Some(c) =
                            chars_iter.next_if(|c| ('a'..='z').contains(c) || ('A'..='Z').contains(c) || c == &'_')
                        {
                            lit.push(c);
                        }
                        if VALID_STACK_KEYWORDS.contains(&lit.as_str()) {
                            TokenKind::StackKeyword(lit)
                        } else if lit.as_str() == "@" {
                            TokenKind::At
                        } else {
                            return lexer_error!(
                                LexerErrorKind::InvalidStackKeyword,
                                file.path.clone(),
                                SourceInfo::new(line_number + 1, line_offset, lit.len()),
                                tokens
                            );
                        }
                    },
                    '+' => TokenKind::Plus,
                    '^' => TokenKind::Dereference,
                    '.' => {
                        if let Some(_) = chars_iter.next_if_eq(&'.') {
                            if let Some(_) = chars_iter.next_if_eq(&'.') {
                                TokenKind::DotDotDot
                            } else {
                                return lexer_error!(
                                    LexerErrorKind::UnexpectedChar,
                                    file.path.clone(),
                                    SourceInfo::new(line_number + 1, line_offset + 1, 1),
                                    tokens
                                );
                            }
                        } else {
                            return lexer_error!(
                                LexerErrorKind::UnexpectedChar,
                                file.path.clone(),
                                SourceInfo::new(line_number + 1, line_offset + 1, 1),
                                tokens
                            );
                        }
                    },
                    ':' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::CopyAssign
                        } else {
                            if let Some(_) = chars_iter.next_if_eq(&'>') {
                                TokenKind::CopyCall
                            } else {
                                TokenKind::Colon
                            }
                        }
                    },
                    '-' => {
                        if let Some(_) = chars_iter.next_if_eq(&'>') {
                            TokenKind::Arrow
                        } else {
                            TokenKind::Minus
                        }
                    },
                    '*' => TokenKind::Star,
                    '/' => {
                        if let Some(_) = chars_iter.next_if_eq(&'/') {
                            while let Some(_) = chars_iter.next_if(|char| *char != '\n') {}
                            continue;
                        } else {
                            TokenKind::Slash
                        }
                    },
                    '=' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::Equal
                        } else {
                            return lexer_error!(
                                LexerErrorKind::UnexpectedChar,
                                file.path.clone(),
                                SourceInfo::new(line_number + 1, line_offset + 1, 1),
                                tokens
                            );
                        }
                    }, // ==
                    '!' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::NotEqual
                        } else {
                            TokenKind::Not
                        }
                    }, // ! or !=
                    '<' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::LessEqual
                        } else {
                            TokenKind::Less
                        }
                    }, // < or <=
                    '>' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::GreaterEqual
                        } else {
                            TokenKind::Greater
                        }
                    }, // > or >=
                    '&' => {
                        if let Some(_) = chars_iter.next_if_eq(&'&') {
                            TokenKind::And
                        } else {
                            if let Some(_) = chars_iter.next_if_eq(&'=') {
                                TokenKind::MoveAssign
                            } else {
                                if let Some(_) = chars_iter.next_if_eq(&'>') {
                                    TokenKind::MoveCall
                                } else {
                                    TokenKind::AddressOf
                                }
                            }
                        } // &&
                    },
                    '|' => {
                        if let Some(_) = chars_iter.next_if_eq(&'|') {
                            TokenKind::Or
                        } else {
                            return lexer_error!(
                                LexerErrorKind::UnexpectedChar,
                                file.path.clone(),
                                SourceInfo::new(line_number + 1, line_offset + 1, 1),
                                tokens
                            );
                        }
                    }, // ||

                    '"' => {
                        let mut lit = String::new();
                        loop {
                            if let Some(_) = chars_iter.next_if_eq(&'"') {
                                break;
                            } else {
                                if let Some(c) = chars_iter.next() {
                                    line_offset += 1;
                                    if c == '\\' {
                                        match chars_iter.next() {
                                            Some('n') => lit.push('\n'),
                                            Some('t') => lit.push('\t'),
                                            Some('\\') => lit.push('\\'),
                                            Some('"') => lit.push('"'),
                                            Some(c) => {
                                                lit.push('\\');
                                                lit.push(c);
                                            }, // unknown escape
                                            None => {
                                                return lexer_error!(
                                                    LexerErrorKind::UnexpectedEof,
                                                    file.path.clone(),
                                                    SourceInfo::new(line_number + 1, line_offset + 1, 1),
                                                    tokens
                                                )
                                            },
                                        }
                                    } else {
                                        lit.push(c);
                                    }
                                }
                            }
                        }
                        line_offset -= lit.len();
                        TokenKind::Literal(Literal::String(lit))
                    },
                    '0'..='9' => {
                        let mut lit = char.to_string();
                        while let Some(c) = chars_iter.next_if(|c| ('0'..='9').contains(c) || c == &'.') {
                            lit.push(c);
                        }
                        // funny use of result types
                        let number: Result<i32, f32> = match lit.parse::<i32>() {
                            Ok(number) => Ok(number),
                            Err(_) => match lit.parse::<f32>() {
                                Ok(number) => Err(number),
                                Err(_) => panic!("[internal error] number conversion error"),
                            },
                        };
                        match number {
                            Ok(int) => TokenKind::Literal(Literal::Integer(int)),
                            Err(float) => TokenKind::Literal(Literal::Float(float)),
                        }
                    },
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let mut lit = char.to_string();
                        while let Some(c) = chars_iter.next_if(|c| {
                            ('a'..='z').contains(c) || ('A'..='Z').contains(c) || ('0'..='9').contains(c) || c == &'_'
                        }) {
                            lit.push(c);
                        }
                        match lit.as_str() {
                            "let" => TokenKind::Let,
                            "if" => TokenKind::If,
                            "else" => TokenKind::Else,
                            "while" => TokenKind::While,
                            "break" => TokenKind::Break,
                            "continue" => TokenKind::Continue,
                            "return" => TokenKind::Return,
                            "fn" => TokenKind::Fn,

                            "i32" => TokenKind::Int,
                            "str" => TokenKind::String,
                            "f32" => TokenKind::Float,
                            "bool" => TokenKind::Bool,
                            "true" => TokenKind::Literal(Literal::Bool(true)),
                            "false" => TokenKind::Literal(Literal::Bool(false)),

                            _ => TokenKind::Identifier(lit),
                        }
                    },
                    _ => {
                        return lexer_error!(
                            LexerErrorKind::UnexpectedChar,
                            file.path.clone(),
                            SourceInfo::new(line_number + 1, line_offset, 1),
                            tokens
                        )
                    },
                };
                let width = kind.get_width();
                tokens.push(Token::new(kind.clone(), SourceInfo::new(line_number + 1, line_offset, width)));
                line_offset += width;
            }
        }
        Ok(tokens)
    }
}
