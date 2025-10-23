use crate::ast_node::*;

impl<'a> Typed for Operation<'a> {
    fn get_type(&self) -> Type {
        match self {
            Operation::Add(expression, expression1) => todo!(),
            Operation::Subtract(expression, expression1) => todo!(),
            Operation::Multiply(expression, expression1) => todo!(),
            Operation::Divide(expression, expression1) => todo!(),
            Operation::Negate(expression, expression1) => todo!(),
            Operation::Or(expression, expression1) => todo!(),
            Operation::And(expression, expression1) => todo!(),
            Operation::Not(expression) => todo!(),
        }
    }
}
impl<'a> Typed for Expression<'a> {
    fn get_type(&self) -> Type {
        match self {
            Expression::Operation(operation) => todo!(),
            Expression::Assignment(expression) => todo!(),
            Expression::Identifier(identifier) => todo!(),
        }
    }
}

impl<'a> Typed for Statement<'a> {
    fn get_type(&self) -> Type {
        match self {
            Statement::If(if_stmt) => todo!(),
            Statement::Return => todo!(),
            Statement::StackBlock(stack) => todo!(),
            Statement::While(expression, expressions) => todo!(),
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
        }
    }
}

impl<'a> Typed for TopLevelNode<'a> {
    fn get_type(&self) -> Type {
        match self {
            TopLevelNode::Statement(statement) => todo!(),
            TopLevelNode::VarDeclaration(expression) => todo!(),
            TopLevelNode::FunctionDeclaration(expressions) => todo!(),
        }
    }
}
