use crate::ast_node::*;

impl<'a> Analyze for Declaration<'a> {
    fn analyze(&self) {
        match self {
            Declaration::VarDeclaration(expression, expression1) => todo!(),
            Declaration::FunctionDeclaration(expressions) => todo!(),
        }
    }
}

impl Analyze for Operation<'_> {
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
impl<'a> Analyze for Expression<'a> {

    fn analyze(&self) {
        match self {
            Expression::Operation(operation) => todo!(),
            Expression::Assignment(_) => todo!(),
            Expression::Identifier(identifier) => todo!(),
        }
    }
}

impl<'a> Analyze for Statement<'a> {

    fn analyze(&self) {
        match self {
            Statement::If(if_stmt) => todo!(),
            Statement::Return(return_stmt) => todo!(),
            Statement::StackBlocks() => todo!(),
        }
    }
}

impl<'a> Analyze for TopLevelNode<'a> {

    fn analyze(&self) {
        match self {
            TopLevelNode::Declaration(declaration) => todo!(),
            TopLevelNode::Statement(statement) => todo!(),
        }
    }
}


struct Analyzer<'a> {
    nodes: Vec<TopLevelNode<'a>>
}

fn analyze_file() {

}


