use crate::hfs::ast::*;
use crate::hfs::token::*;

impl<'a> AstArena<'a> {
    // Convenience methods for common types
    pub fn int_type(&self) -> TypeId {
        TypeId(0)
    }
    pub fn float_type(&self) -> TypeId {
        TypeId(1)
    }
    pub fn bool_type(&self) -> TypeId {
        TypeId(2)
    }
    pub fn string_type(&self) -> TypeId {
        TypeId(3)
    }

    // Only expressions have types!
    pub fn get_type_of_operation(&mut self, op: &Operation) -> TypeId {
        match op {
            // Arithmetic operations: return the operand type
            Operation::Add(lhs, rhs)
            | Operation::Sub(lhs, rhs)
            | Operation::Mul(lhs, rhs)
            | Operation::Div(lhs, rhs)
            | Operation::Mod(lhs, rhs) => {
                let lhs_type = self.get_type_id_of_expr(*lhs);
                let rhs_type = self.get_type_id_of_expr(*rhs);
                // TODO: Type checking - they should match
                lhs_type
            }
            Operation::Or(_, _)
            | Operation::And(_, _)
            | Operation::Not(_)
            | Operation::Equal(_, _)
            | Operation::NotEqual(_, _)
            | Operation::Less(_, _)
            | Operation::LessEqual(_, _)
            | Operation::Greater(_, _)
            | Operation::GreaterEqual(_, _) => self.bool_type(),
        }
    }

    pub fn get_type_id_of_expr(&mut self, expr_id: ExprId) -> TypeId {
        match self.get_expr(expr_id).clone() {
            Expression::Operation(operation) => self.get_type_of_operation(&operation),
            Expression::Identifier(identifier) => match identifier {
                Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => {
                    let var = self.get_var(var_id);
                    var.hfs_type
                }
                Identifier::Function(func_id) => {
                    let func = self.get_func(func_id);
                    func.return_type
                }
            },
            Expression::Literal(literal) => match literal {
                Literal::Integer(_) => self.int_type(),
                Literal::Float(_) => self.float_type(),
                Literal::String(_) => self.string_type(),
                Literal::Bool(_) => self.bool_type(),
            },
            Expression::Tuple { expressions } => {
                let token = self.get_expr_token(expr_id).clone();
                // Build tuple type from element types
                let mut element_types = Vec::new();
                for expr_id in expressions.clone() {
                    let elem_type = self.get_type_id_of_expr(expr_id);
                    element_types.push(elem_type);
                }

                let tuple_type = Type::Tuple(element_types);
                self.alloc_type(tuple_type, token)
            }
            Expression::Parameter { index, type_id } => type_id,
            Expression::ReturnValue(type_id) => type_id,
            Expression::StackKeyword { .. } => todo!(),
        }
    }
    pub fn get_type_of_var(&self, var_id: VarId) -> &Type {
        self.get_type(self.get_var(var_id).hfs_type)
    }
    pub fn get_type_of_func(&self, func_id: FuncId) -> &Type {
        self.get_type(self.get_func(func_id).return_type)
    }

    pub fn get_type_of_expr(&mut self, expr_id: ExprId) -> &Type {
        let type_id = self.get_type_id_of_expr(expr_id);
        self.get_type(type_id)
    }
}
