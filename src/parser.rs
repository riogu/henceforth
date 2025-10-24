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
    fn expect(&mut self, token_kind: TokenKind) -> TokenKind {
        match self.tokens.next() {
            Some(val) if val.kind == token_kind => val.kind,
            Some(_) | None => {
                panic!("expected '{}'", token_kind)
            }
        }
    }
}

impl<'a> Parser<'a> {
    // declarations
    fn function_declaration(&mut self) {}
    fn variable_declaration(&mut self) {
        // let var: i32;
        // expect(identifier) | expect(:) | expect(identifier) | expect(;)
        let var = TokenKind::Identifier("foo".to_string());
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
        todo!()
        // if let Some(token) = self.tokens.peek() {
        //     match token.kind {
        //         TokenKind::If => todo!(),
        //         TokenKind::At => todo!("stack block"),
        //         TokenKind::Return => todo!(),
        //         _ => panic!("expected statement"),
        //     }
        // }
    }
    fn stack_block(&mut self) {}
}
