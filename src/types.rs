use crate::ast_node::*;

impl Typed for Operation {
    fn get_type(&self) -> Type {
        match self {
            Operation::Add(lhs, rhs) => todo!(),
            Operation::Subtract(lhs, rhs) => todo!(),
            Operation::Multiply(lhs, rhs) => todo!(),
            Operation::Divide(lhs, rhs) => todo!(),
            Operation::Negate(expr) => todo!(),
            Operation::Or(lhs, rhs) => todo!(),
            Operation::And(lhs, rhs) => todo!(),
            Operation::Not(expr) => todo!(),
        }
    }
}

impl Typed for Expression {
    fn get_type(&self) -> Type {
        match self {
            Expression::Operation(operation) => todo!(),
            Expression::CopyAssignment(lhs, rhs) => todo!(),
            Expression::MoveAssignment(lhs, rhs) => todo!(),
            Expression::Identifier(identifier) => todo!(),
            Expression::Literal(literal) => todo!(),
            Expression::FunctionCall(function_id, args) => todo!(),
        }
    }
}

impl Typed for Statement {
    fn get_type(&self) -> Type {
        match self {
            Statement::If{cond, body, else_stmt} => todo!(),
            Statement::Return => todo!(),
            Statement::StackBlock(stack_block) => todo!(),
            Statement::BlockScope(block_scope) => todo!(),
            Statement::While{cond, body} => todo!(),
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Empty => todo!(),
        }
    }
}

impl Typed for TopLevelId {
    fn get_type(&self) -> Type {
        match self {
            TopLevelId::VariableDecl(var_id) => todo!(),
            TopLevelId::FunctionDecl(fn_id) => todo!(),
            TopLevelId::Statement(stmt_id) => todo!(),
        }
    }
}
