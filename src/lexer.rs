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
                            ('a'..='z').contains(c)
                                || ('A'..='Z').contains(c)
                                || ('0'..='9').contains(c)
                                || c == &'_'
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

                            "i32" => TokenKind::Int,
                            "str" => TokenKind::String,
                            "f32" => TokenKind::Float,
                            "bool" => TokenKind::Bool,
                            "true" => TokenKind::Literal(Literal::Bool(true)),
                            "false" => TokenKind::Literal(Literal::Bool(false)),

                            _ => TokenKind::Identifier(lit),
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

    use crate::{
        ast_node::Type,
        builder::{FunctionOps, StackOps, TokenSequence},
    };

    use super::*;

    fn tokenize_file_into_kinds(path: &str) -> Vec<TokenKind> {
        let path = PathBuf::from(path);
        let file = File::new(&path);
        Lexer::new()
            .tokenize(&file)
            .into_iter()
            .map(|token| token.kind)
            .collect()
    }

    #[test]
    fn test_tokenize_simple_main() {
        let tokens = tokenize_file_into_kinds("test/simple_main.hfs");

        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .return_statement()
            .end_body()
            .build();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_function_with_lots_of_arguments() {
        let tokens = tokenize_file_into_kinds("test/function_with_lots_of_arguments.hfs");
        let expected = TokenSequence::new()
            .func_with(
                "func_with_lots_of_arguments",
                Some(vec![Type::Int, Type::Float, Type::String, Type::Bool]),
                Some(vec![Type::Int, Type::Float, Type::Bool, Type::String]),
            )
            .body()
            .stack_block()
            .push_literal(5)
            .push_literal(5.0)
            .push_literal(false)
            .push_literal("test")
            .end_stack_block()
            .return_statement()
            .end_body()
            .build();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_function_with_no_arguments() {
        let tokens = tokenize_file_into_kinds("test/function_with_no_arguments.hfs");
        let expected = TokenSequence::new()
            .func_with("no_args", None, Some(vec![Type::Int]))
            .body()
            .stack_block()
            .push_literal(4)
            .end_stack_block()
            .return_statement()
            .end_body()
            .build();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_function_with_no_return_type() {
        let tokens = tokenize_file_into_kinds("test/function_with_no_return_type.hfs");
        let expected = TokenSequence::new()
            .func_with("no_return_type", Some(vec![Type::Int]), None)
            .body()
            .return_statement()
            .end_body();
    }
}
