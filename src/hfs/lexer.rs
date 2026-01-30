use std::{fs, path::PathBuf};

use crate::hfs::{
    error::{CompileError, LexerError},
    lexer,
    token::{Literal, SourceInfo, Token, TokenKind},
    VALID_STACK_KEYWORDS,
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
                    ' ' | '\t' | '\r' => {
                        line_offset += 1;
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
                            panic!("lexer error")
                        }
                    },
                    '+' => TokenKind::Plus,
                    '.' =>
                        if let Some(_) = chars_iter.next_if_eq(&'.') {
                            if let Some(_) = chars_iter.next_if_eq(&'.') {
                                TokenKind::DotDotDot
                            } else {
                                panic!("lexer error")
                            }
                        } else {
                            panic!("lexer error")
                        },
                    ':' =>
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::CopyAssign
                        } else {
                            if let Some(_) = chars_iter.next_if_eq(&'>') {
                                TokenKind::CopyCall
                            } else {
                                TokenKind::Colon
                            }
                        },
                    '-' =>
                        if let Some(_) = chars_iter.next_if_eq(&'>') {
                            TokenKind::Arrow
                        } else {
                            TokenKind::Minus
                        },
                    '*' => TokenKind::Star,
                    '/' =>
                        if let Some(_) = chars_iter.next_if_eq(&'/') {
                            continue;
                        } else {
                            TokenKind::Slash
                        },
                    '=' =>
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::Equal
                        } else {
                            return Err(Box::new(LexerError::UnexpectedChar {
                                path: file.path.clone(),
                                source_info: SourceInfo::new(line_number + 1, line_offset + 1, 1),
                            }));
                        }, // ==
                    '!' =>
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::NotEqual
                        } else {
                            TokenKind::Not
                        }, // ! or !=
                    '<' =>
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::LessEqual
                        } else {
                            TokenKind::Less
                        }, // < or <=
                    '>' =>
                        if let Some(_) = chars_iter.next_if_eq(&'=') {
                            TokenKind::GreaterEqual
                        } else {
                            TokenKind::Greater
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
                                    panic!("lexer error")
                                }
                            }
                        } // &&
                    },
                    '|' =>
                        if let Some(_) = chars_iter.next_if_eq(&'|') {
                            TokenKind::Or
                        } else {
                            panic!("lexer error")
                        }, // ||

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
                    },
                    '0'..='9' => {
                        let mut lit = char.to_string();
                        while let Some(c) = chars_iter.next_if(|c| ('0'..='9').contains(c) || c == &'.') {
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
                    _ => panic!("lexer error"),
                };
                let width = kind.get_width();
                line_offset += width;
                tokens.push(Token::new(kind.clone(), SourceInfo::new(line_number, line_offset, width)));
            }
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::*;
    use crate::hfs::{
        ast::Type,
        builder::builder::{Builder, BuilderOperation, ControlFlowOps, FunctionOps, LoopOps, PassMode, StackOps, VariableOps},
        lexer_builder::TokenSequence,
        utils::{run_until, Phase},
    };

    pub fn tokenize_file_into_kinds(name: &str) -> Vec<TokenKind> {
        run_until(name, Phase::Lexer)
            .expect("compilation failed")
            .as_any()
            .downcast_ref::<Vec<Token>>()
            .expect("Expected Vec<Token> from Lexer")
            .clone()
            .into_iter()
            .map(|token| token.kind)
            .collect()
    }

    #[test]
    fn test_simple_main() {
        let tokens = tokenize_file_into_kinds("test/simple_main.hfs");
        let expected = TokenSequence::new().func_with("main", None, None).body().end_body().build();

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
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal(5)
            .push_literal(5.0)
            .push_literal(false)
            .push_literal("test")
            .end_stack_block(true)
            .end_body()
            .func_with("main", None, None)
            .body()
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
            .end_stack_block(true)
            .end_body()
            .func_with("main", None, None)
            .body()
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
            .push_stack_keyword("@pop", true)
            .end_body()
            .func_with("main", None, None)
            .body()
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
            .assign_to("copy", PassMode::Copy)
            .assign_to("move", PassMode::Move)
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
            .push_operation(BuilderOperation::Add)
            .push_literal(2)
            .push_operation(BuilderOperation::Multiply)
            .end_stack_block(true)
            .push_stack_keyword("@dup", true)
            .stack_block()
            .push_operation(BuilderOperation::Divide)
            .push_literal(2)
            .push_operation(BuilderOperation::Multiply)
            .push_literal(2)
            .push_operation(BuilderOperation::Modulo)
            .end_stack_block(true)
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal(false)
            .end_stack_block(true)
            .push_stack_keyword("@dup", true)
            .stack_block()
            .push_operation(BuilderOperation::Or)
            .push_operation(BuilderOperation::Not)
            .push_literal(true)
            .push_operation(BuilderOperation::And)
            .end_stack_block(true)
            .push_stack_keyword("@pop", true)
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
            .assign_to("a", PassMode::Move)
            .stack_block()
            .push_literal(0)
            .end_stack_block(false)
            .assign_to("b", PassMode::Move)
            .while_loop()
            .stack_block()
            .push_variable("a")
            .push_literal(0)
            .push_operation(BuilderOperation::GreaterThan)
            .push_variable("b")
            .push_literal(200)
            .push_operation(BuilderOperation::LessThan)
            .push_operation(BuilderOperation::And)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_variable("a")
            .push_literal(1)
            .push_operation(BuilderOperation::Subtract)
            .end_stack_block(false)
            .assign_to("a", PassMode::Move)
            .stack_block()
            .push_variable("b")
            .push_literal(2)
            .push_operation(BuilderOperation::Add)
            .end_stack_block(false)
            .assign_to("b", PassMode::Move)
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
            .push_operation(BuilderOperation::GreaterThan)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal(1)
            .push_literal(2)
            .push_literal(3)
            .push_literal(4)
            .end_stack_block(true)
            .push_stack_keyword("@pop_all", true)
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
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("fizzbuzz")
            .end_stack_block(true)
            .end_body()
            .elif_statement()
            .stack_block()
            .push_literal(3)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("fizz")
            .end_stack_block(true)
            .end_body()
            .elif_statement()
            .stack_block()
            .push_literal(5)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("buzz")
            .end_stack_block(true)
            .end_body()
            .else_statement()
            .body()
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal("no fizzbuzz")
            .end_stack_block(true)
            .end_body()
            .end_body()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(530)
            .end_stack_block(false)
            .call_function("fizz_buzz", PassMode::Move)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_copy_and_move_func_calls() {
        let tokens = tokenize_file_into_kinds("test/copy_and_move_func_calls.hfs");
        let expected = TokenSequence::new()
            .func_with("max", Some(vec![Type::Int, Type::Int]), Some(vec![Type::Int]))
            .body()
            .if_statement()
            .stack_block()
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@dup", false)
            .push_stack_keyword("@rot", false)
            .push_operation(BuilderOperation::GreaterThan)
            .end_stack_block(false)
            .body()
            .return_statement()
            .end_body()
            .else_statement()
            .body()
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", true)
            .end_body()
            .end_body()
            .func_with("max3", Some(vec![Type::Int, Type::Int, Type::Int]), Some(vec![Type::Int]))
            .body()
            .push_stack_keyword("@rrot", true)
            .call_function("max", PassMode::Move)
            .call_function("max", PassMode::Copy)
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", false)
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(50)
            .push_literal(10)
            .push_literal(30)
            .end_stack_block(false)
            .call_function("max3", PassMode::Move)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(tokens, expected)
    }
}
