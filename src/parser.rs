use crate::ast_node::TopLevelNode;
use crate::token::Token;
use crate::token::TokenKind;

use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser<'a> {
    nodes: Vec<TopLevelNode<'a>>,
    tokens: Peekable<IntoIter<Token<'a>>>,  // Own the tokens, iterate by value
}

impl<'a> Parser<'a> {
    // utils
    fn push(&mut self, node: TopLevelNode<'a>) {
        self.nodes.push(node);
    }
}

impl<'a> Parser<'a> { // recursive descent parser
    #[must_use]
    pub fn parse_tokens(tokens: Vec<Token<'a>>) -> Vec<TopLevelNode<'a>> {
        let parser = Parser{nodes: Vec::new(), tokens: tokens.into_iter().peekable()};

    }
    #[must_use]
    fn statement(&mut self) {
        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Literal(literal) => todo!(),
                TokenKind::Let => todo!(),
                TokenKind::If => todo!(),
                TokenKind::Else => todo!(),
                TokenKind::Elif => todo!(),
                TokenKind::While => todo!(),
                TokenKind::Break => todo!(),
                TokenKind::Continue => todo!(),
                TokenKind::Return => todo!(),
                TokenKind::Fn => todo!(),
                TokenKind::IntT => todo!(),
                TokenKind::StringT => todo!(),
                TokenKind::BoolT => todo!(),
                TokenKind::FloatT => todo!(),
                TokenKind::Plus => todo!(),
                TokenKind::Minus => todo!(),
                TokenKind::Star => todo!(),
                TokenKind::Slash => todo!(),
                TokenKind::Percent => todo!(),
                TokenKind::Equal => todo!(),
                TokenKind::NotEqual => todo!(),
                TokenKind::Less => todo!(),
                TokenKind::LessEqual => todo!(),
                TokenKind::Greater => todo!(),
                TokenKind::GreaterEqual => todo!(),
                TokenKind::And => todo!(),
                TokenKind::Or => todo!(),
                TokenKind::Not => todo!(),
                TokenKind::CopyAssign => todo!(),
                TokenKind::MoveAssign => todo!(),
                TokenKind::LeftParen => todo!(),
                TokenKind::RightParen => todo!(),
                TokenKind::LeftBrace => todo!(),
                TokenKind::RightBrace => todo!(),
                TokenKind::LeftBracket => todo!(),
                TokenKind::RightBracket => todo!(),
                TokenKind::At => todo!(),
                TokenKind::Semicolon => todo!(),
                TokenKind::Comma => todo!(),
                TokenKind::Dot => todo!(),
                TokenKind::Colon => todo!(),
                TokenKind::Arrow => todo!(),
                TokenKind::Newline => todo!(),
                TokenKind::Eof => todo!(),
            }
        }
    }
    #[must_use]
    fn variable_declaration(&mut self) {}
    #[must_use]
    fn function_declaration(&mut self) {}
    #[must_use]
    fn stack_block(&mut self) {}
}
