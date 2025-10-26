use crate::hfs::ast::*;

impl<'a> AstArena<'a> {
    pub fn get_type_of_operation(&self, op: &Operation) -> Type {
        match op {
            Operation::Add(lhs, rhs) => todo!(),
            Operation::Sub(lhs, rhs) => todo!(),
            Operation::Mul(lhs, rhs) => todo!(),
            Operation::Div(lhs, rhs) => todo!(),
            Operation::Mod(lhs, rhs) => todo!(),
            Operation::Or(lhs, rhs) => todo!(),
            Operation::And(lhs, rhs) => todo!(),
            Operation::Not(expr) => todo!(),
            Operation::Equal(lhs, rhs) => todo!(),
            Operation::Less(lhs, rhs) => todo!(),
            Operation::LessEqual(lhs, rhs) => todo!(),
            Operation::Greater(lhs, rhs) => todo!(),
            Operation::GreaterEqual(lhs, rhs) => todo!(),
            Operation::NotEqual(lhs, rhs) => todo!(),
        }
    }

    pub fn get_type_of_expr(&self, expr_id: ExprId) -> Type {
        let expr = self.get_expr(expr_id);
        match expr {
            Expression::Operation(operation) => self.get_type_of_operation(operation),
            Expression::Identifier(identifier) => {
                        todo!()
                    }
            Expression::Literal(literal) => todo!(),
            Expression::FunctionCall { tuple, identifier } => todo!(),
            Expression::Tuple { expressions, variadic } => todo!(),
            Expression::Parameter(_) => todo!(),
        }
    }

    pub fn get_type_of_stmt(&self, stmt_id: StmtId) -> Type {
        let stmt = self.get_stmt(stmt_id);
        match stmt {
            Statement::If { cond, body, else_stmt } => todo!(),
            Statement::Return => todo!(),
            Statement::StackBlock(stack_block) => todo!(),
            Statement::BlockScope(block_scope) => todo!(),
            Statement::While { cond, body } => todo!(),
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Assignment { value, identifier, is_move } => todo!(),
            Statement::Empty => todo!(),
        }
    }

    pub fn get_type_of_top_level(&self, top_level_id: TopLevelId) -> Type {
        match top_level_id {
            TopLevelId::VariableDecl(var_id) => todo!(),
            TopLevelId::FunctionDecl(fn_id) => todo!(),
            TopLevelId::Statement(stmt_id) => todo!(),
        }
    }
}
