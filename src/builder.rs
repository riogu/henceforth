use crate::{
    ast_node::Type,
    token::{Literal, TokenKind},
};

pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Not,
    Or,
    And,
    GreaterThan,
    GreaterThanEq,
    Equals,
    NotEquals,
    LessThan,
    LessThanEq,
}

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
    fn end_stack_block(self, semicolon: bool) -> Self;
    fn push_literal<T: Into<Literal>>(self, lit: T) -> Self;
    fn push_operation(self, op: Operation) -> Self;
    fn push_function<I>(self, name: &str, args: I, expected_args: usize) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Literal>;
    fn push_function_implicit(self, name: &str) -> Self;
    fn push_variable(self, name: &str) -> Self;
}

pub trait VariableOps {
    fn variable(self, name: &str, typename: Type) -> Self;
    fn move_to(self, name: &str) -> Self;
    fn copy_to(self, name: &str) -> Self;
}

pub trait LoopOps {
    fn while_loop(self) -> Self;
}

pub trait ControlFlowOps {
    fn if_statement(self) -> Self;
    fn elif_statement(self) -> Self;
    fn else_statement(self) -> Self;
}

impl ControlFlowOps for TokenSequence {
    fn if_statement(self) -> Self {
        self.push(TokenKind::If)
    }

    fn elif_statement(self) -> Self {
        self.push(TokenKind::Else).push(TokenKind::If)
    }

    fn else_statement(self) -> Self {
        self.push(TokenKind::Else)
    }
}

impl LoopOps for TokenSequence {
    fn while_loop(self) -> Self {
        self.push(TokenKind::While)
    }
}

impl VariableOps for TokenSequence {
    fn variable(self, name: &str, typename: Type) -> Self {
        self.push(TokenKind::Let)
            .push(TokenKind::Identifier(name.to_string()))
            .push(TokenKind::Colon)
            .push(typename.into())
            .push(TokenKind::Semicolon)
    }

    fn move_to(self, name: &str) -> Self {
        self.push(TokenKind::MoveAssign)
            .push(TokenKind::Identifier(name.to_string()))
            .push(TokenKind::Semicolon)
    }

    fn copy_to(self, name: &str) -> Self {
        self.push(TokenKind::CopyAssign)
            .push(TokenKind::Identifier(name.to_string()))
            .push(TokenKind::Semicolon)
    }
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
                .map(|typename| typename.into())
                .collect::<Vec<TokenKind>>();
            return self.push_many(tokens);
        }
        self
    }
    fn return_types(mut self, return_types: Option<Vec<Type>>) -> Self {
        if let Some(return_types) = return_types {
            let tokens = return_types
                .into_iter()
                .map(|typename| typename.into())
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
            .push(TokenKind::Identifier(name.to_string()))
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

    fn end_stack_block(mut self, semicolon: bool) -> Self {
        self = self.push(TokenKind::RightParen);
        if semicolon {
            self = self.push(TokenKind::Semicolon);
        }
        self
    }

    fn push_literal<T: Into<Literal>>(mut self, lit: T) -> Self {
        self.push(TokenKind::Literal(lit.into()))
    }

    fn push_operation(mut self, op: Operation) -> Self {
        self.push(op.into())
    }

    fn push_function<I>(mut self, name: &str, args: I, expected_args: usize) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Literal>,
    {
        self = self.push(TokenKind::LeftParen);
        let args: Vec<Literal> = args.into_iter().map(|arg| arg.into()).collect();
        if args.len() < expected_args {
            self = self.push(TokenKind::DotDotDot);
        }
        for arg in args {
            self = self.push(TokenKind::Literal(arg));
        }
        self.push(TokenKind::RightParen)
            .push(TokenKind::Identifier(name.to_string()))
    }
    fn push_function_implicit(mut self, name: &str) -> Self {
        self.push(TokenKind::LeftParen)
            .push(TokenKind::DotDotDot)
            .push(TokenKind::RightParen)
            .push(TokenKind::Identifier(name.to_string()))
    }

    fn push_variable(self, name: &str) -> Self {
        self.push(TokenKind::Identifier(name.to_string()))
    }
}
