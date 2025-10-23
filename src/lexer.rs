use crate::token::Literal;
use crate::token::SourceInfo;
use crate::token::Token;
use crate::token::TokenKind;
use std::{fs, path::PathBuf};

#[derive(Debug)]
pub struct File<'a> {
    contents: Vec<String>,
    path: &'a PathBuf,
}

macro_rules! empty_void_func_tokens {
    ($name:ident) => {
        TokenKind::Fn,
        TokenKind::Literal(Literal::Identifier(String::from(stringify!($name)))),
        TokenKind::Colon,
        TokenKind::LeftParen,
        TokenKind::RightParen,
        TokenKind::Arrow,
        TokenKind::LeftParen,
        TokenKind::RightParen,
        TokenKind::LeftBrace,
        TokenKind::Return,
        TokenKind::Semicolon,
        TokenKind::RightBrace,
    }
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
                    ' ' | '\t' | '\r' => continue,
                    '(' => TokenKind::LeftParen,
                    ')' => TokenKind::RightParen,
                    '{' => TokenKind::LeftBrace,
                    '}' => TokenKind::RightBrace,
                    ';' => TokenKind::Semicolon,
                    '%' => TokenKind::Percent,
                    '@' => TokenKind::At,
                    '+' => TokenKind::Plus,
                    ':' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::CopyAssign
                        } else {
                            TokenKind::Colon
                        }
                    }
                    '-' => {
                        if let Some(_) = chars_iter.next_if_eq(&'>') {
                            TokenKind::Arrow
                        } else {
                            TokenKind::Minus
                        }
                    }
                    '*' => TokenKind::Star,
                    '/' => {
                        if let Some(_) = chars_iter.next_if_eq(&'/') {
                            continue;
                        } else {
                            TokenKind::Slash
                        }
                    }
                    '=' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::Equal
                        } else {
                            panic!("lexer error")
                        }
                    } // ==
                    '!' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::NotEqual
                        } else {
                            TokenKind::Not
                        }
                    } // ! or !=
                    '<' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::LessEqual
                        } else {
                            TokenKind::Less
                        }
                    } // < or <=
                    '>' => {
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::GreaterEqual
                        } else {
                            TokenKind::Greater
                        }
                    } // > or >=
                    '&' => {
                        if let Some(_) = chars_iter.next_if_eq(&'&') {
                            TokenKind::And
                        } else {
                            if let Some(_) = chars_iter.next_if_eq(&'=') {
                                TokenKind::MoveAssign
                            } else {
                                panic!("lexer error")
                            }
                        } // &&
                    }
                    '|' => {
                        if let Some(_) = chars_iter.next_if_eq(&'|') {
                            TokenKind::Or
                        } else {
                            panic!("lexer error")
                        }
                    } // ||

                    '"' => {
                        let mut lit = char.to_string();
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
                        TokenKind::Literal(Literal::String(lit))
                    }
                    '0'..='9' => {
                        let mut lit = char.to_string();
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
                            Ok(int) => TokenKind::Literal(Literal::Integer(int)),
                            Err(float) => TokenKind::Literal(Literal::Float(float)),
                        }
                    }
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let mut lit = char.to_string();
                        while let Some(c) = chars_iter.next_if(|c| {
                            ('a'..='z').contains(c) || ('A'..='Z').contains(c) || c == &'_'
                        }) {
                            lit.push(c);
                        }
                        match lit.as_str() {
                            "let" => TokenKind::Let,
                            "if" => TokenKind::If,
                            "else" => TokenKind::Else,
                            "elif" => TokenKind::Elif,
                            "while" => TokenKind::While,
                            "break" => TokenKind::Break,
                            "continue" => TokenKind::Continue,
                            "return" => TokenKind::Return,
                            "fn" => TokenKind::Fn,

                            "i32" => TokenKind::IntT,
                            "string" => TokenKind::StringT,
                            "f32" => TokenKind::FloatT,
                            "bool" => TokenKind::BoolT,

                            _ => TokenKind::Literal(Literal::Identifier(lit)),
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

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::*;

    #[test]
    fn test_tokenize_kind_simple_main() {
        let path = PathBuf::from("test/simple_main.hfs");
        let file = File::new(&path);

        let lexer = Lexer::new();
        let tokens: Vec<TokenKind> = lexer
            .tokenize(&file)
            .into_iter()
            .map(|token| token.kind)
            .collect();

        let expected = vec![empty_void_func_tokens!(main)];
        assert_eq!(tokens, expected);
    }
}
