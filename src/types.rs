use crate::ast_node::*;

impl Typed for Operation {
    fn get_type(&self) -> Type {
        match self {
            Operation::Add(_, lhs, rhs) => todo!(),
            Operation::Subtract(_, lhs, rhs) => todo!(),
            Operation::Multiply(_, lhs, rhs) => todo!(),
            Operation::Divide(_, lhs, rhs) => todo!(),
            Operation::Negate(_, expr) => todo!(),
            Operation::Or(_, lhs, rhs) => todo!(),
            Operation::And(_, lhs, rhs) => todo!(),
            Operation::Not(_, expr) => todo!(),
        }
    }
}

impl Typed for Expression {
    fn get_type(&self) -> Type {
        match self {
            Expression::Operation(_, operation) => todo!(),
            Expression::Assignment(_, lhs, rhs) => todo!(),
            Expression::Identifier(_, identifier) => todo!(),
            Expression::Literal(_, literal) => todo!(),
            Expression::FunctionCall(_, function_id, args) => todo!(),
        }
    }
}

impl Typed for Statement {
    fn get_type(&self) -> Type {
        match self {
            Statement::If(if_stmt) => todo!(),
            Statement::Return(token) => todo!(),
            Statement::StackBlock(stack_block) => todo!(),
            Statement::BlockScope(block_scope) => todo!(),
            Statement::While(while_stmt) => todo!(),
            Statement::Break(token) => todo!(),
            Statement::Continue(token) => todo!(),
            Statement::Empty(token) => todo!(),
        }
    }
}

impl Typed for TopLevelNode {
    fn get_type(&self) -> Type {
        match self {
            TopLevelNode::VariableDecl(var_id) => todo!(),
            TopLevelNode::FunctionDecl(fn_id) => todo!(),
            TopLevelNode::Statement(stmt_id) => todo!(),
        }
    }
}
