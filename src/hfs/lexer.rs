use crate::hfs::token::Literal;
use crate::hfs::token::SourceInfo;
use crate::hfs::token::Token;
use crate::hfs::token::TokenKind;
use std::{fs, path::PathBuf};

#[derive(Debug)]
pub struct File<'a> {
    pub contents: Vec<String>,
    pub path: &'a PathBuf,
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

pub struct Lexer {} // idk how to make this a namespace

impl Lexer {
    #[must_use]
    pub fn tokenize<'a>(file: &'a File) -> Vec<Token<'a>> {
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
                    '.' => {
                        if let Some(_) = chars_iter.next_if_eq(&'.') {
                            if let Some(_) = chars_iter.next_if_eq(&'.') {
                                TokenKind::DotDotDot
                            } else {
                                panic!("lexer error")
                            }
                        } else {
                            panic!("lexer error")
                        }
                    }
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
        hfs::ast_node::Type,
        hfs::builder::{
            ControlFlowOps, FunctionOps, LoopOps, Operation, StackOps, TokenSequence, VariableOps,
        },
    };

    use super::*;

    fn tokenize_file_into_kinds(path: &str) -> Vec<TokenKind> {
        let path = PathBuf::from(path);
        let file = File::new(&path);
        Lexer::tokenize(&file)
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
            .push_function_implicit("pop")
            .push_function_implicit("pop")
            .push_function_implicit("pop")
            .push_function_implicit("pop")
            .end_stack_block(true)
            .stack_block()
            .push_literal(5)
            .push_literal(5.0)
            .push_literal(false)
            .push_literal("test")
            .end_stack_block(false)
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
            .end_stack_block(false)
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
            .stack_block()
            .push_function_implicit("pop")
            .end_stack_block(false)
            .return_statement()
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_variable_declarations() {
        let tokens = tokenize_file_into_kinds("test/variable_declarations.hfs");
        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .variable("a", Type::Int)
            .variable("b", Type::Float)
            .variable("c", Type::String)
            .variable("d", Type::Bool)
            .return_statement()
            .end_body()
            .build();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_copy_and_move() {
        let tokens = tokenize_file_into_kinds("test/copy_and_move.hfs");
        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .variable("copy", Type::Int)
            .variable("move", Type::Int)
            .stack_block()
            .push_literal(5)
            .end_stack_block(false)
            .copy_to("copy")
            .move_to("move")
            .return_statement()
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_operations() {
        let tokens = tokenize_file_into_kinds("test/operations.hfs");
        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(1)
            .push_literal(1)
            .push_operation(Operation::Add)
            .push_literal(2)
            .push_operation(Operation::Multiply)
            .push_function_implicit("dup")
            .push_operation(Operation::Divide)
            .push_literal(2)
            .push_operation(Operation::Multiply)
            .push_literal(2)
            .push_operation(Operation::Modulo)
            .push_function_implicit("pop")
            .end_stack_block(true)
            .stack_block()
            .push_function("dup", vec![false], 1)
            .push_operation(Operation::Or)
            .push_operation(Operation::Not)
            .push_literal(true)
            .push_operation(Operation::And)
            .push_function_implicit("pop")
            .end_stack_block(true)
            .return_statement()
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_while_loop() {
        let tokens = tokenize_file_into_kinds("test/while_loop.hfs");
        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .variable("a", Type::Int)
            .variable("b", Type::Int)
            .stack_block()
            .push_literal(100)
            .end_stack_block(false)
            .move_to("a")
            .stack_block()
            .push_literal(0)
            .end_stack_block(false)
            .move_to("b")
            .while_loop()
            .stack_block()
            .push_variable("a")
            .push_literal(0)
            .push_operation(Operation::GreaterThan)
            .push_variable("b")
            .push_literal(200)
            .push_operation(Operation::LessThan)
            .push_operation(Operation::And)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_variable("a")
            .push_literal(1)
            .push_operation(Operation::Subtract)
            .end_stack_block(false)
            .move_to("a")
            .stack_block()
            .push_variable("b")
            .push_literal(2)
            .push_operation(Operation::Add)
            .end_stack_block(false)
            .move_to("b")
            .end_body()
            .return_statement()
            .end_body()
            .build();
        assert_eq!(tokens, expected)
    }

    #[test]
    fn test_simple_if_else() {
        let tokens = tokenize_file_into_kinds("test/simple_if_else.hfs");
        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .if_statement()
            .stack_block()
            .push_literal(5)
            .push_literal(2)
            .push_operation(Operation::GreaterThan)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal(1)
            .push_literal(2)
            .push_literal(3)
            .push_literal(4)
            .push_function::<Vec<i32>>("pop_all", vec![], 0)
            .end_stack_block(true)
            .return_statement()
            .end_body()
            .else_statement()
            .body()
            .return_statement()
            .end_body()
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_if_elif_else() {
        let tokens = tokenize_file_into_kinds("test/if_elif_else.hfs");
        let expected = TokenSequence::new()
            .func_with("fizz_buzz", Some(vec![Type::Int]), Some(vec![Type::String]))
            .body()
            .if_statement()
            .stack_block()
            .push_literal(15)
            .push_operation(Operation::Modulo)
            .push_literal(0)
            .push_operation(Operation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("fizzbuzz")
            .end_stack_block(false)
            .return_statement()
            .end_body()
            .elif_statement()
            .stack_block()
            .push_literal(3)
            .push_operation(Operation::Modulo)
            .push_literal(0)
            .push_operation(Operation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("fizz")
            .end_stack_block(false)
            .return_statement()
            .end_body()
            .elif_statement()
            .stack_block()
            .push_literal(5)
            .push_operation(Operation::Modulo)
            .push_literal(0)
            .push_operation(Operation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("buzz")
            .end_stack_block(false)
            .return_statement()
            .end_body()
            .else_statement()
            .body()
            .stack_block()
            .push_function_implicit("pop")
            .push_literal("no fizzbuzz")
            .end_stack_block(false)
            .return_statement()
            .end_body()
            .end_body()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_function("fizz_buzz", vec![530], 1)
            .push_function_implicit("pop")
            .end_stack_block(false)
            .return_statement()
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_partially_implicit_function_calls() {
        let tokens = tokenize_file_into_kinds("test/partially_implicit_function_calls.hfs");
        let expected = TokenSequence::new()
            .func_with(
                "max",
                Some(vec![Type::Int, Type::Int]),
                Some(vec![Type::Int]),
            )
            .body()
            .if_statement()
            .stack_block()
            .push_function_implicit("swap")
            .push_function_implicit("dup")
            .push_function_implicit("rot")
            .push_operation(Operation::GreaterThan)
            .end_stack_block(false)
            .body()
            .return_statement()
            .end_body()
            .else_statement()
            .body()
            .stack_block()
            .push_function_implicit("swap")
            .push_function_implicit("pop")
            .end_stack_block(false)
            .return_statement()
            .end_body()
            .end_body()
            .func_with(
                "max3",
                Some(vec![Type::Int, Type::Int, Type::Int]),
                Some(vec![Type::Int]),
            )
            .body()
            .stack_block()
            .push_function_implicit("rot")
            .push_function_implicit("rot")
            .push_function_implicit("max")
            .push_function_implicit("max")
            .end_stack_block(false)
            .return_statement()
            .end_body()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(50)
            .end_stack_block(true)
            .stack_block()
            .push_function("max3", vec![10, 30], 3)
            .push_function_implicit("pop")
            .end_stack_block(false)
            .return_statement()
            .end_body()
            .build();
        assert_eq!(tokens, expected)
    }
}
