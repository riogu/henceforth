use crate::ast_node::*;

impl Typed for Operation {
    fn get_type(&self) -> Type {
        match self {
            Operation::Add(expression, expression1) => todo!(),
            Operation::Subtract(expression, expression1) => todo!(),
            Operation::Multiply(expression, expression1) => todo!(),
            Operation::Divide(expression, expression1) => todo!(),
            Operation::Negate(expression) => todo!(),
            Operation::Or(expression, expression1) => todo!(),
            Operation::And(expression, expression1) => todo!(),
            Operation::Not(expression) => todo!(),
        }
    }
}
impl Typed for Expression {
    fn get_type(&self) -> Type {
        match self {
            Expression::Operation(operation) => todo!(),
            Expression::Assignment(expression, expression1) => todo!(),
            Expression::Identifier(identifier) => todo!(),
            Expression::Literal(_) => todo!(),
            Expression::FunctionCall(function_id, expr_ids) => todo!(),
        }
    }
}

impl Typed for Statement {
    fn get_type(&self) -> Type {
        match self {
            Statement::If(if_stmt) => todo!(),
            Statement::Return => todo!(),
            Statement::StackBlock(stack) => todo!(),
            Statement::BlockScope(scope_block) => todo!(),
            Statement::While(expression, expressions) => todo!(),
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Empty => todo!(),
        }
    }
}

impl Typed for TopLevelNode {
    fn get_type(&self) -> Type {
        match self {
            TopLevelNode::VariableDecl(expression) => todo!(),
            TopLevelNode::FunctionDecl(expressions) => todo!(),
            TopLevelNode::Statement(statement) => todo!(),
        }
    }
}
