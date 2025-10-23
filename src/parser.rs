use crate::ast_node::TopLevelNode;
use crate::token::Literal;
use crate::token::Token;
use crate::token::TokenKind;

use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser<'a> {
    nodes: Vec<TopLevelNode<'a>>,
    tokens: Peekable<IntoIter<Token<'a>>>, // Own the tokens, iterate by value
}

impl<'a> Parser<'a> {
    // utils
    fn push(&mut self, node: TopLevelNode<'a>) {
        self.nodes.push(node);
    }
}

impl<'a> Parser<'a> {
    // recursive descent parser
    #[must_use]
    pub fn parse_tokens(tokens: Vec<Token<'a>>) -> Vec<TopLevelNode<'a>> {
        let mut parser = Parser {
            nodes: Vec::new(),
            tokens: tokens.into_iter().peekable(),
        };
        while let Some(token) = parser.tokens.peek() {
            match &token.kind {
                TokenKind::Let => parser.variable_declaration(),
                TokenKind::Fn => parser.function_declaration(),
                _ => panic!("expected variable declaration or function declaration."),
            }
        }
        return parser.nodes;
    }

    fn function_declaration(&mut self) {}
    fn variable_declaration(&mut self) {}
    fn statement(&mut self) {
        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::If => todo!(),
                TokenKind::At => todo!("stack block"),
                TokenKind::Return => todo!(),
                _ => panic!("expected variable declaration or function declaration."),
            }
        }
    }
    fn stack_block(&mut self) {}
}
