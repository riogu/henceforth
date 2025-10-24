use crate::ast_node::*;
use crate::token::*;

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
    fn expect(&mut self, token_kind: TokenKind) -> Token {
        match self.tokens.next() {
            Some(token) if token.kind == token_kind => token,
            Some(_) | None => panic!("expected '{}'", token_kind),
            // TODO: replace panic with source location info and arrow thingy
        }
    }
}

impl<'a> Parser<'a> {
    // declarations
    fn function_declaration(&mut self) {}
    fn variable_declaration(&mut self) {
        // let var: i32;
        // expect(identifier) | expect(:) | expect(identifier) | expect(;)
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
                _ => panic!("expected variable or function declaration"),
            }
        }
        return parser.nodes;
    }
    fn statement(&mut self) -> Statement<'a> {
        let Some(token) = self.tokens.next() else { panic!("unexpected end of input") };

        match token.kind {
            TokenKind::If => todo!(),
            TokenKind::At => todo!("stack block"),
            TokenKind::Return => todo!(),
            _ => panic!("expected statement"),
        }
    }
    fn stack_block(&mut self) {}
}
