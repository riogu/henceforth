use crate::hfs::{
    token::{Literal, TokenKind},
    Type, UnresolvedOperation,
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
impl BuilderOperation {
    pub fn to_unresolved_op(&self) -> UnresolvedOperation {
        match self {
            BuilderOperation::Add => UnresolvedOperation::Add,
            BuilderOperation::Subtract => UnresolvedOperation::Sub,
            BuilderOperation::Multiply => UnresolvedOperation::Mul,
            BuilderOperation::Divide => UnresolvedOperation::Div,
            BuilderOperation::Modulo => UnresolvedOperation::Mod,
            BuilderOperation::Not => UnresolvedOperation::Not,
            BuilderOperation::Or => UnresolvedOperation::Or,
            BuilderOperation::And => UnresolvedOperation::And,
            BuilderOperation::GreaterThan => UnresolvedOperation::Greater,
            BuilderOperation::GreaterThanEq => UnresolvedOperation::GreaterEqual,
            BuilderOperation::Equals => UnresolvedOperation::Equal,
            BuilderOperation::NotEquals => UnresolvedOperation::NotEqual,
            BuilderOperation::LessThan => UnresolvedOperation::Less,
            BuilderOperation::LessThanEq => UnresolvedOperation::LessEqual,
        }
    }
}

pub enum PassMode {
    Copy,
    Move,
}

pub trait Builder<T>: FunctionOps + StackOps + VariableOps + LoopOps + ControlFlowOps {
    type Built;
    fn new() -> Self;
    fn push(self, elem: T) -> Self;
    fn push_many(self, elems: Vec<T>) -> Self;
    fn build(self) -> Self::Built;
}

pub trait FunctionOps {
    fn args(self, args: Option<Vec<Type>>) -> Self;
    fn return_types(self, return_types: Option<Vec<Type>>) -> Self;
    fn func_with(self, name: &str, args: Option<Vec<Type>>, return_type: Option<Vec<Type>>) -> Self;
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
