use crate::hfs::{ast::*, error::CompileError, token::*, CfgOperation, InstId, Instruction, IrArena, IrFuncId, IrVarId};

pub const PRIMITIVE_TYPE_COUNT: usize = 4;

impl AstArena {
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
    pub fn get_type_of_operation(&mut self, op: &Operation) -> Result<TypeId, Box<dyn CompileError>> {
        match op {
            Operation::Add(lhs, rhs)
            | Operation::Sub(lhs, rhs)
            | Operation::Mul(lhs, rhs)
            | Operation::Div(lhs, rhs)
            | Operation::Mod(lhs, rhs) => {
                let lhs_type = self.get_type_id_of_expr(*lhs)?;
                let rhs_type = self.get_type_id_of_expr(*rhs)?;
                self.compare_types(lhs_type, rhs_type, vec![self.get_expr_token(*lhs).clone()])?;
                Ok(lhs_type)
            },
            Operation::Or(_, _)
            | Operation::And(_, _)
            | Operation::Not(_)
            | Operation::Equal(_, _)
            | Operation::NotEqual(_, _)
            | Operation::Less(_, _)
            | Operation::LessEqual(_, _)
            | Operation::Greater(_, _)
            | Operation::GreaterEqual(_, _) => Ok(self.bool_type()),
            Operation::AddressOf(expr_id) => Ok(todo!()),
            Operation::Dereference(expr_id) => Ok(todo!()),
        }
    }

    pub fn get_type_id_of_expr(&mut self, expr_id: ExprId) -> Result<TypeId, Box<dyn CompileError>> {
        match self.get_expr(expr_id).clone() {
            Expression::Operation(operation) => Ok(self.get_type_of_operation(&operation)?),
            Expression::Identifier(identifier) => match identifier {
                Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => {
                    let var = self.get_var(var_id);
                    Ok(var.hfs_type)
                },
                Identifier::Function(func_id) => {
                    let func = self.get_func(func_id);
                    Ok(func.return_type)
                },
            },
            Expression::Literal(literal) => match literal {
                Literal::Integer(_) => Ok(self.int_type()),
                Literal::Float(_) => Ok(self.float_type()),
                Literal::String(_) => Ok(self.string_type()),
                Literal::Bool(_) => Ok(self.bool_type()),
            },
            Expression::Tuple { expressions } => {
                let token = self.get_expr_token(expr_id).clone();
                // Build tuple type from element types
                let mut element_types = Vec::new();
                for expr_id in expressions.clone() {
                    let elem_type = self.get_type_id_of_expr(expr_id)?;
                    element_types.push(elem_type);
                }

                let tuple_type = Type::Tuple { type_ids: element_types, ptr_count: 0 };
                Ok(self.alloc_type(tuple_type, token))
            },
            Expression::Parameter { index, type_id } => Ok(type_id),
            Expression::StackKeyword { .. } => Ok(todo!()),
            Expression::ReturnValue(type_id) => Ok(type_id),
        }
    }
    pub fn get_type_of_var(&self, var_id: VarId) -> &Type {
        self.get_type(self.get_var(var_id).hfs_type)
    }
    pub fn get_type_of_func(&self, func_id: FuncId) -> &Type {
        self.get_type(self.get_func(func_id).return_type)
    }

    pub fn get_type_of_expr(&mut self, expr_id: ExprId) -> Result<&Type, Box<dyn CompileError>> {
        let type_id = self.get_type_id_of_expr(expr_id)?;
        Ok(self.get_type(type_id))
    }
}

impl IrArena {
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
    pub fn get_type_of_operation(&mut self, op: &CfgOperation) -> TypeId {
        match op {
            // Arithmetic operations: return the operand type
            CfgOperation::Add(lhs, rhs)
            | CfgOperation::Sub(lhs, rhs)
            | CfgOperation::Mul(lhs, rhs)
            | CfgOperation::Div(lhs, rhs)
            | CfgOperation::Mod(lhs, rhs) => {
                let lhs_type = self.get_type_id_of_inst(*lhs);
                let rhs_type = self.get_type_id_of_inst(*rhs);
                if let Err(err) = self.compare_types(lhs_type, rhs_type) {
                    panic!("{}", err)
                }
                lhs_type
            },
            CfgOperation::Or(_, _)
            | CfgOperation::And(_, _)
            | CfgOperation::Not(_)
            | CfgOperation::Equal(_, _)
            | CfgOperation::NotEqual(_, _)
            | CfgOperation::Less(_, _)
            | CfgOperation::LessEqual(_, _)
            | CfgOperation::Greater(_, _)
            | CfgOperation::GreaterEqual(_, _) => self.bool_type(),
        }
    }

    pub fn get_type_id_of_inst(&mut self, inst_id: InstId) -> TypeId {
        match self.get_instruction(inst_id).clone() {
            Instruction::Operation(source_info, operation) => self.get_type_of_operation(&operation),
            Instruction::Literal(source_info, literal) => match literal {
                Literal::Integer(_) => self.int_type(),
                Literal::Float(_) => self.float_type(),
                Literal::String(_) => self.string_type(),
                Literal::Bool(_) => self.bool_type(),
            },
            Instruction::Tuple { source_info, instructions } => {
                // Build tuple type from element types
                let mut element_types = Vec::new();
                for inst_id in instructions.clone() {
                    let elem_type = self.get_type_id_of_inst(inst_id);
                    element_types.push(elem_type);
                }

                let tuple_type = Type::Tuple { type_ids: element_types, ptr_count: 0 };
                self.alloc_type(tuple_type, source_info.clone())
            },
            Instruction::Parameter { source_info, index, type_id } => type_id,
            Instruction::FunctionCall { source_info, args, func_id, is_move, return_values } =>
                self.get_func(func_id).return_type,
            Instruction::Phi { source_info, incoming } => todo!(),
            Instruction::StackKeyword { .. } => todo!(),
            Instruction::LoadElement { source_info, index, tuple } => todo!(),
            Instruction::ReturnValue { source_info, type_id } => type_id,
            Instruction::Load { source_info, address } => todo!(),
            Instruction::Store { source_info, address, value } => todo!(),
            Instruction::Alloca { source_info, type_id } => todo!(),
        }
    }
    pub fn get_type_of_var(&self, var_id: IrVarId) -> &Type {
        self.get_type(self.get_var(var_id).hfs_type)
    }
    pub fn get_type_of_func(&self, func_id: IrFuncId) -> &Type {
        self.get_type(self.get_func(func_id).return_type)
    }

    pub fn get_type_of_inst(&mut self, inst_id: InstId) -> &Type {
        let type_id = self.get_type_id_of_inst(inst_id);
        self.get_type(type_id)
    }
}
