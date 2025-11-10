use crate::{
    hfs::token::{Literal, TokenKind},
    hfs::Type,
};

pub enum BuilderOperation {
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

pub enum PassMode {
    Copy,
    Move,
}

pub trait Builder<T> {
    fn new() -> Self;
    fn push(self, kind: T) -> Self;
    fn push_many(self, kinds: Vec<T>) -> Self;
    fn build(self) -> Vec<T>;
}

pub trait FunctionOps {
    fn args(self, args: Option<Vec<Type>>) -> Self;
    fn return_types(self, return_types: Option<Vec<Type>>) -> Self;
    fn func_with(self, name: &str, args: Option<Vec<Type>>, return_type: Option<Vec<Type>>)
        -> Self;
    fn body(self) -> Self;
    fn end_body(self) -> Self;
    fn call_function(self, name: &str, mode: PassMode) -> Self;
    fn return_statement(self) -> Self;
}

pub trait StackOps {
    fn stack_block(self) -> Self;
    fn end_stack_block(self, semicolon: bool) -> Self;
    fn push_literal<T: Into<Literal>>(self, lit: T) -> Self;
    fn push_operation(self, op: BuilderOperation) -> Self;
    fn push_stack_keyword(self, keyword: &str, semicolon: bool) -> Self;
    fn push_variable(self, name: &str) -> Self;
}

pub trait VariableOps {
    fn variable(self, name: &str, typename: Type) -> Self;
    fn assign_to(self, name: &str, mode: PassMode) -> Self;
}

pub trait LoopOps {
    fn while_loop(self) -> Self;
}

pub trait ControlFlowOps {
    fn if_statement(self) -> Self;
    fn elif_statement(self) -> Self;
    fn else_statement(self) -> Self;
}
