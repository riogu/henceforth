use crate::hfs::{ast::*, error::CompileError, token::*, GlobalIrVarId, InstId, Instruction, IrArena, IrFuncId, IrOperation};

pub const PRIMITIVE_TYPE_COUNT: usize = 4;

impl AstArena {
    // Convenience methods for common types
    pub fn int_type(&self) -> TypeId { TypeId(0) }
    pub fn float_type(&self) -> TypeId { TypeId(1) }
    pub fn bool_type(&self) -> TypeId { TypeId(2) }
    pub fn string_type(&self) -> TypeId { TypeId(3) }

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
            Operation::AddressOf(_) => todo!(),
            Operation::Dereference(_) => todo!(),
            Operation::ArrayAccess(expr_id, expr_id1) => todo!(),
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
            Expression::Parameter { index: _, type_id } => Ok(type_id),
            Expression::ReturnValue(type_id) => Ok(type_id),
        }
    }
    pub fn get_type_of_var(&self, var_id: VarId) -> &Type { self.get_type(self.get_var(var_id).hfs_type) }
    pub fn get_type_of_func(&self, func_id: FuncId) -> &Type { self.get_type(self.get_func(func_id).return_type) }

    pub fn get_type_of_expr(&mut self, expr_id: ExprId) -> Result<&Type, Box<dyn CompileError>> {
        let type_id = self.get_type_id_of_expr(expr_id)?;
        Ok(self.get_type(type_id))
    }
}

impl IrArena {
    // Convenience methods for common types
    pub fn int_type(&self) -> TypeId { TypeId(0) }
    pub fn float_type(&self) -> TypeId { TypeId(1) }
    pub fn bool_type(&self) -> TypeId { TypeId(2) }
    pub fn string_type(&self) -> TypeId { TypeId(3) }

    // Only expressions have types!
    pub fn get_type_of_operation(&mut self, op: &IrOperation) -> Result<TypeId, Box<dyn CompileError>> {
        match op {
            // Arithmetic operations: return the operand type
            IrOperation::Add(lhs, rhs)
            | IrOperation::Sub(lhs, rhs)
            | IrOperation::Mul(lhs, rhs)
            | IrOperation::Div(lhs, rhs)
            | IrOperation::Mod(lhs, rhs) => {
                let lhs_type = self.get_type_id_of_inst(*lhs)?;
                let rhs_type = self.get_type_id_of_inst(*rhs)?;
                self.compare_types(lhs_type, rhs_type, vec![self.get_inst(*lhs).get_source_info().clone()])?;
                Ok(lhs_type)
            },
            IrOperation::Or(_, _)
            | IrOperation::And(_, _)
            | IrOperation::Not(_)
            | IrOperation::Equal(_, _)
            | IrOperation::NotEqual(_, _)
            | IrOperation::Less(_, _)
            | IrOperation::LessEqual(_, _)
            | IrOperation::Greater(_, _)
            | IrOperation::GreaterEqual(_, _) => Ok(self.bool_type()),
        }
    }

    pub fn get_type_id_of_inst(&mut self, inst_id: InstId) -> Result<TypeId, Box<dyn CompileError>> {
        match self.get_inst(inst_id).clone() {
            Instruction::Operation { source_info: _, op } => Ok(self.get_type_of_operation(&op)?),
            Instruction::Literal { source_info: _, literal } => match literal {
                Literal::Integer(_) => Ok(self.int_type()),
                Literal::Float(_) => Ok(self.float_type()),
                Literal::String(_) => Ok(self.string_type()),
                Literal::Bool(_) => Ok(self.bool_type()),
            },
            Instruction::Tuple { source_info, instructions } => {
                // Build tuple type from element types
                let mut element_types = Vec::new();
                for inst_id in instructions.clone() {
                    let elem_type = self.get_type_id_of_inst(inst_id)?;
                    element_types.push(elem_type);
                }

                let tuple_type = Type::Tuple { type_ids: element_types, ptr_count: 0 };
                Ok(self.alloc_type(tuple_type, source_info.clone()))
            },
            Instruction::Parameter { source_info: _, index: _, type_id } => Ok(type_id),
            Instruction::FunctionCall { source_info: _, args: _, func_id, is_move: _, return_values: _ } =>
                Ok(self.get_func(func_id).return_type),
            Instruction::Phi { source_info: _, incoming } => Ok(self.get_type_id_of_inst(
                *incoming.values().next().expect("[internal error] found phi with no elements in type checking"),
            )?),
            Instruction::LoadElement { source_info: _, index: _, tuple: _ } => todo!(),
            Instruction::ReturnValue { source_info: _, type_id } => Ok(type_id),
            Instruction::Load { source_info: _, address: _, type_id } => Ok(type_id),
            Instruction::Store { source_info: _, address: _, value } => Ok(self.get_type_id_of_inst(value)?),
            Instruction::Alloca { source_info: _, type_id: _ } => {
                // implement this later
                panic!("[internal error] asked for the type of an alloca instruction but i don't see why this would happen")
            },
            Instruction::GlobalAlloca(_) => todo!(),
        }
    }
    pub fn get_type_of_var(&self, var_id: GlobalIrVarId) -> &Type { self.get_type(self.get_var(var_id).hfs_type) }
    pub fn get_type_of_func(&self, func_id: IrFuncId) -> &Type { self.get_type(self.get_func(func_id).return_type) }

    pub fn get_type_of_inst(&mut self, inst_id: InstId) -> Result<&Type, Box<dyn CompileError>> {
        let type_id = self.get_type_id_of_inst(inst_id)?;
        Ok(self.get_type(type_id))
    }

    // returns itself with the pointer count reduced by one
    pub fn reduce_type_ptr_count(&mut self, type_id: TypeId, source_info: SourceInfo) -> TypeId {
        let hfs_type = match self.get_type(type_id) {
            Type::Int { ptr_count } => Type::Int { ptr_count: ptr_count - 1 },
            Type::String { ptr_count } => Type::String { ptr_count: ptr_count - 1 },
            Type::Bool { ptr_count } => Type::Bool { ptr_count: ptr_count - 1 },
            Type::Float { ptr_count } => Type::Float { ptr_count: ptr_count - 1 },
            Type::Tuple { ptr_count, type_ids } => Type::Tuple { ptr_count: ptr_count - 1, type_ids: type_ids.clone() },
            Type::Array { hfs_type, length } => todo!(),
        };
        self.alloc_type(hfs_type, source_info)
    }

    // these functions are only used for printing so we don't have to pass a mutable reference to an arena everywhere
    pub fn get_type_of_operation_no_alloc(&self, op: &IrOperation) -> Option<TypeId> {
        match op {
            IrOperation::Add(lhs, rhs)
            | IrOperation::Sub(lhs, rhs)
            | IrOperation::Mul(lhs, rhs)
            | IrOperation::Div(lhs, rhs)
            | IrOperation::Mod(lhs, rhs) => self.get_type_id_of_inst_no_alloc(*lhs),
            IrOperation::Or(_, _)
            | IrOperation::And(_, _)
            | IrOperation::Not(_)
            | IrOperation::Equal(_, _)
            | IrOperation::NotEqual(_, _)
            | IrOperation::Less(_, _)
            | IrOperation::LessEqual(_, _)
            | IrOperation::Greater(_, _)
            | IrOperation::GreaterEqual(_, _) => Some(self.bool_type()),
        }
    }

    pub fn get_type_id_of_inst_no_alloc(&self, inst_id: InstId) -> Option<TypeId> {
        match self.get_inst(inst_id) {
            Instruction::Operation { op, .. } => self.get_type_of_operation_no_alloc(op),
            Instruction::Literal { literal, .. } => match literal {
                Literal::Integer(_) => Some(self.int_type()),
                Literal::Float(_) => Some(self.float_type()),
                Literal::String(_) => Some(self.string_type()),
                Literal::Bool(_) => Some(self.bool_type()),
            },
            Instruction::Tuple { instructions, .. } => {
                let element_types: Option<Vec<TypeId>> =
                    instructions.iter().map(|id| self.get_type_id_of_inst_no_alloc(*id)).collect();
                let element_types = element_types?;
                self.types.iter().enumerate().find_map(|(i, t)| {
                    if let Type::Tuple { type_ids, .. } = t {
                        if *type_ids == element_types {
                            Some(TypeId(i))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
            },
            Instruction::Parameter { type_id, .. } => Some(*type_id),
            Instruction::FunctionCall { func_id, .. } => Some(self.get_func(*func_id).return_type),
            Instruction::Phi { incoming, .. } => self.get_type_id_of_inst_no_alloc(*incoming.values().next()?),
            Instruction::ReturnValue { type_id, .. } => Some(*type_id),
            Instruction::Load { type_id, .. } => Some(*type_id),
            Instruction::Store { value, .. } => self.get_type_id_of_inst_no_alloc(*value),
            Instruction::Alloca { .. } => None,
            Instruction::LoadElement { .. } => None,
            Instruction::GlobalAlloca(_) => None,
        }
    }
}
