use crate::ast_node::*;

impl Typed for Operation {
    fn get_type(&self) -> Type {
        match self {
            Operation::Add(lhs, rhs) => todo!(),
            Operation::Sub(lhs, rhs) => todo!(),
            Operation::Mul(lhs, rhs) => todo!(),
            Operation::Div(lhs, rhs) => todo!(),
            Operation::Mod(expr_id, expr_id1) => todo!(),
            Operation::Negate(expr) => todo!(),
            Operation::Or(lhs, rhs) => todo!(),
            Operation::And(lhs, rhs) => todo!(),
            Operation::Not(expr) => todo!(),
            Operation::Equal(expr_id, expr_id1) => todo!(),
            Operation::Less(expr_id, expr_id1) => todo!(),
            Operation::LessEqual(expr_id, expr_id1) => todo!(),
            Operation::Greater(expr_id, expr_id1) => todo!(),
            Operation::GreaterEqual(expr_id, expr_id1) => todo!(),
            Operation::NotEqual(expr_id, expr_id1) => todo!(),
        }
    }
}

impl Typed for Expression {
    fn get_type(&self) -> Type {
        match self {
            Expression::Operation(operation) => todo!(),
            Expression::Identifier(identifier) => todo!(),
            Expression::Literal(literal) => todo!(),
            Expression::FunctionCall{tuple, identifier} => todo!(),
            Expression::Tuple(expr_ids) => todo!(),
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
            Statement::Assignment { value, identifier } => todo!(),
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
