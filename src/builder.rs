use crate::{
    ast_node::Type,
    token::{Literal, TokenKind},
};

pub struct TokenSequence {
    tokens: Vec<TokenKind>,
}

pub trait FunctionOps {
    fn args(self, args: Option<Vec<Type>>) -> Self;
    fn return_types(self, return_types: Option<Vec<Type>>) -> Self;
    fn func_with(self, name: &str, args: Option<Vec<Type>>, return_type: Option<Vec<Type>>)
        -> Self;
    fn body(self) -> Self;
    fn end_body(self) -> Self;
    fn return_statement(self) -> Self;
}

pub trait StackOps {
    fn stack_block(self) -> Self;
    fn end_stack_block(self) -> Self;
    fn push_literal<T: Into<Literal>>(self, lit: T) -> Self;
}

impl TokenSequence {
    pub fn new() -> Self {
        Self { tokens: Vec::new() }
    }

    fn push(mut self, kind: TokenKind) -> Self {
        self.tokens.push(kind);
        self
    }

    fn push_many(mut self, kinds: Vec<TokenKind>) -> Self {
        for token in kinds {
            self.tokens.push(token);
        }
        self
    }

    pub fn build(self) -> Vec<TokenKind> {
        self.tokens
    }
}

impl FunctionOps for TokenSequence {
    fn args(mut self, args: Option<Vec<Type>>) -> Self {
        if let Some(args) = args {
            let tokens = args
                .into_iter()
                .map(|typename| typename.to_token())
                .collect::<Vec<TokenKind>>();
            return self.push_many(tokens);
        }
        self
    }
    fn return_types(mut self, return_types: Option<Vec<Type>>) -> Self {
        if let Some(return_types) = return_types {
            let tokens = return_types
                .into_iter()
                .map(|typename| typename.to_token())
                .collect::<Vec<TokenKind>>();
            return self.push_many(tokens);
        }
        self
    }

    fn func_with(
        mut self,
        name: &str,
        args: Option<Vec<Type>>,
        return_type: Option<Vec<Type>>,
    ) -> Self {
        self.push(TokenKind::Fn)
            .push(TokenKind::Literal(Literal::Identifier(String::from(name))))
            .push(TokenKind::Colon)
            .push(TokenKind::LeftParen)
            .args(args)
            .push(TokenKind::RightParen)
            .push(TokenKind::Arrow)
            .push(TokenKind::LeftParen)
            .return_types(return_type)
            .push(TokenKind::RightParen)
    }

    fn body(mut self) -> Self {
        self.push(TokenKind::LeftBrace)
    }

    fn end_body(mut self) -> Self {
        self.push(TokenKind::RightBrace)
    }

    fn return_statement(mut self) -> Self {
        self.push(TokenKind::Return).push(TokenKind::Semicolon)
    }
}

impl StackOps for TokenSequence {
    fn stack_block(mut self) -> Self {
        self.push(TokenKind::At).push(TokenKind::LeftParen)
    }

    fn end_stack_block(mut self) -> Self {
        self.push(TokenKind::RightParen)
    }

    fn push_literal<T: Into<Literal>>(mut self, lit: T) -> Self {
        self.push(TokenKind::Literal(lit.into()))
    }
}
