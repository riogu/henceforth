use crate::hfs::{
    builder::builder::{Builder, BuilderOperation, ControlFlowOps, FunctionOps, LoopOps, PassMode, StackOps, VariableOps},
    Literal, TokenKind, Type,
};

pub struct TokenSequence {
    tokens: Vec<TokenKind>,
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

    fn assign_to(self, name: &str, mode: PassMode) -> Self {
        match mode {
            PassMode::Copy => self.push(TokenKind::CopyAssign),
            PassMode::Move => self.push(TokenKind::MoveAssign),
        }
        .push(TokenKind::Identifier(name.to_string()))
        .push(TokenKind::Semicolon)
    }
}

impl Builder<TokenKind> for TokenSequence {
    type Built = Vec<TokenKind>;
    fn new() -> Self {
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

    fn build(self) -> Vec<TokenKind> {
        self.tokens
    }
}

impl FunctionOps for TokenSequence {
    fn args(mut self, args: Option<Vec<Type>>) -> Self {
        if let Some(args) = args {
            let tokens = args.into_iter().map(|typename| typename.into()).collect::<Vec<TokenKind>>();
            return self.push_many(tokens);
        }
        self
    }
    fn return_types(mut self, return_types: Option<Vec<Type>>) -> Self {
        if let Some(return_types) = return_types {
            let tokens = return_types.into_iter().map(|typename| typename.into()).collect::<Vec<TokenKind>>();
            return self.push_many(tokens);
        }
        self
    }

    fn func_with(mut self, name: &str, args: Option<Vec<Type>>, return_type: Option<Vec<Type>>) -> Self {
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

    fn call_function(self, name: &str, mode: PassMode) -> Self {
        match mode {
            PassMode::Copy => self.push(TokenKind::CopyCall),
            PassMode::Move => self.push(TokenKind::MoveCall),
        }
        .push(TokenKind::Identifier(name.to_string()))
        .push(TokenKind::Semicolon)
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

    fn push_operation(mut self, op: BuilderOperation) -> Self {
        self.push(op.into())
    }

    fn push_variable(self, name: &str) -> Self {
        self.push(TokenKind::Identifier(name.to_string()))
    }

    fn push_stack_keyword(mut self, keyword: &str, semicolon: bool) -> Self {
        self = self.push(TokenKind::StackKeyword(keyword.to_string()));
        if semicolon {
            self = self.push(TokenKind::Semicolon);
        }
        self
    }
}
