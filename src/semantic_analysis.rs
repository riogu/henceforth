use crate::ast_node::*;

impl<'a> ASTNode for Declaration<'a> {
    fn analyze(&self) {
        match self {
            Declaration::VarDeclaration(expression, expression1) => todo!(),
            Declaration::FunctionDeclaration(expressions) => todo!(),
        }
    }
}

impl ASTNode for Operation<'_> {
    fn analyze(&self) {
        match self {
            Operation::Add(lhs, rhs) => todo!(),
            Operation::Subtract(lhs, rhs) => todo!(),
            Operation::Multiply(lhs, rhs) => todo!(),
            Operation::Divide(lhs, rhs) => todo!(),
            Operation::Negate(lhs, rhs) => todo!(),
            Operation::Or(lhs, rhs) => todo!(),
            Operation::And(lhs, rhs) => todo!(),
            Operation::Not(expr) => todo!(),
        }
    }
}
impl<'a> ASTNode for Expression<'a> {

    fn analyze(&self) {
        match self {
            Expression::Operation(operation) => todo!(),
            Expression::Assignment(_) => todo!(),
            Expression::Identifier(identifier) => todo!(),
        }
    }
}

impl<'a> ASTNode for Statement<'a> {

    fn analyze(&self) {
        match self {
            Statement::If(if_stmt) => todo!(),
            Statement::Return(return_stmt) => todo!(),
            Statement::StackBlocks() => todo!(),
        }
    }
}

impl<'a> ASTNode for RootNode<'a> {

    fn analyze(&self) {
        match self {
            RootNode::Declaration(declaration) => todo!(),
            RootNode::Statement(statement) => todo!(),
        }
    }
}
