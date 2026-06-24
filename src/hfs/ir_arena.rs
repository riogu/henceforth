use core::panic;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
    vec,
};

use colored::{Colorize, CustomColor};
use slotmap::SlotMap;

use crate::{
    hfs::{
        BasicBlock, BlockId, GlobalIrVarDeclaration, GlobalIrVarId, InstId, Instruction, IrFuncId, IrFunction, IrOperation,
        IrType, Literal, Span, TermInstId, TerminatorInst, Type,
        ast::*,
        error::{CompileError, DiagnosticInfo},
        ir_lowerer_errors::IrLowererErrorKind,
        merge_spans,
    },
    ir_lowerer_error,
};

#[derive(Debug, Default)]
pub struct IrArena {
    // TODO: we also need to pass metadata into this new IR
    // which includes expression provenance and etc (for optimizations and annotations)
    // we should make an annotation system and convert into it
    pub global_vars: SlotMap<GlobalIrVarId, GlobalIrVarDeclaration>,
    pub functions: SlotMap<IrFuncId, IrFunction>,
    pub instructions: SlotMap<InstId, Instruction>,
    pub terminators: SlotMap<TermInstId, TerminatorInst>,
    pub blocks: SlotMap<BlockId, BasicBlock>,
    pub types: Vec<IrType>,
    pub func_id_to_blocks: HashMap<IrFuncId, Vec<BlockId>>, // in insertion order

    pub type_spans: Vec<Span>,
    type_cache: HashMap<IrType, TypeId>,
    pub hfs_stack: Vec<InstId>,
    // this is reset whenever we add a terminator instruction to the current block
    // its used to keep track of each merging stack
    pub curr_block_stack: Vec<InstId>,

    //
    // NOTE: this was used by braun et al. we will reuse it for mem2reg later

    // For each variable, in each block, what's the current definition?
    // current_def: HashMap<(IrVarId, BlockId), InstId>,
    // Which blocks have all their predecessors known?
    // sealed_blocks: HashSet<BlockId>,
    // Placeholder phis waiting for block to be sealed
    // incomplete_phis: HashMap<BlockId, HashMap<IrVarId, InstId>>,
    // FIXME: i think i have to clean up incomplete_phis at the end with a second pass that runs
    // once we know all predecessors of every block, otherwise we might not be done with the
    // algorithm (from what i understood)
    pub diagnostic_info: Rc<DiagnosticInfo>,
}

impl IrArena {
    pub fn new(diagnostic_info: Rc<DiagnosticInfo>) -> Self {
        let mut arena = Self::default();
        arena.diagnostic_info = diagnostic_info;
        arena.alloc_type_uncached(IrType::new_int(0), Span::new(0, 0, 0));
        arena.alloc_type_uncached(IrType::new_float(0), Span::new(0, 0, 0));
        arena.alloc_type_uncached(IrType::new_bool(0), Span::new(0, 0, 0));
        arena.alloc_type_uncached(IrType::new_string(0), Span::new(0, 0, 0));
        arena
    }

    // Stack methods (manage the hfs stack for operations)
    pub fn pop_or_error(&mut self, span: Span, ast_arena: &AstArena) -> Result<InstId, Box<dyn CompileError>> {
        match self.pop_hfs_stack() {
            Some(id) => Ok(id),
            None => ir_lowerer_error!(IrLowererErrorKind::StackUnderflow, &*self, Some(ast_arena), span),
        }
    }
    pub fn last_or_error(&mut self, span: Span, ast_arena: &AstArena) -> Result<InstId, Box<dyn CompileError>> {
        match self.hfs_stack.last() {
            Some(id) => Ok(*id),
            None => ir_lowerer_error!(IrLowererErrorKind::ExpectedItemOnStack, &*self, Some(ast_arena), span),
        }
    }
    pub fn push_to_hfs_stack(&mut self, inst: InstId) {
        self.hfs_stack.push(inst);
        // also pushes it to the context used for verifying equality between branches
        self.curr_block_stack.push(inst);
    }
    // use this instead of popping the stack directly because we keep track of other things
    pub fn pop_hfs_stack(&mut self) -> Option<InstId> {
        // also pushes it to the context used for verifying equality between branches
        self.curr_block_stack.pop();
        self.hfs_stack.pop()
    }

    pub fn alloc_type_uncached(&mut self, hfs_type: IrType, span: Span) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type.clone());
        self.type_spans.push(span);
        id
    }
    pub fn alloc_type(&mut self, hfs_type: IrType, span: Span) -> TypeId {
        // Check if this type already exists
        if let Some(&existing_id) = self.type_cache.get(&hfs_type) {
            return existing_id;
        } // If not, allocate it
        self.alloc_type_uncached(hfs_type, span)
    }
    pub fn alloc_function(&mut self, func: IrFunction) -> IrFuncId {
        let id = self.functions.insert(func);
        id
    }

    // used to signal cleanup is needed later
    pub fn invalidate_inst(&mut self, inst_id: InstId) -> Option<Instruction> { self.instructions.remove(inst_id) }
    pub fn inst_is_valid(&self, inst_id: InstId) -> bool { self.instructions.contains_key(inst_id) }

    pub fn alloc_inst_for(&mut self, inst: Instruction, block_id: BlockId) -> InstId {
        let id = self.instructions.insert(inst);
        self.get_block_mut(block_id).instructions.push(id);
        id
    }

    pub fn fill_inst(&mut self, id: InstId, inst: Instruction, block_id: BlockId) {
        self.instructions[id] = inst;
        self.get_block_mut(block_id).instructions.push(id);
    }

    pub fn alloc_inst_at_start_for(&mut self, inst: Instruction, block_id: BlockId) -> InstId {
        let id = self.instructions.insert(inst);
        self.get_block_mut(block_id).instructions.insert(0, id);
        id
    }

    pub fn alloc_global_var(&mut self, var: GlobalIrVarDeclaration) -> (GlobalIrVarId, InstId) {
        let global_var_id = self.global_vars.insert(var);
        let inst_id = self.instructions.insert(Instruction::GlobalAlloca(global_var_id));
        (global_var_id, inst_id)
    }

    pub fn alloc_terminator_for(&mut self, terminator: TerminatorInst, block_id: BlockId) -> TermInstId {
        if self.get_block(block_id).terminator.is_some() {
            // to find bugs in the frontend, this method panics if a terminator existed
            panic!("[internal error] block already had terminator")
        }
        self.alloc_term_impl_unchecked(terminator, block_id)
    }
    pub fn replace_terminator_for(&mut self, terminator: TerminatorInst, block_id: BlockId) -> TermInstId {
        self.remove_terminator_for(block_id);
        self.alloc_term_impl_unchecked(terminator, block_id)
    }
    fn alloc_term_impl_unchecked(&mut self, terminator: TerminatorInst, block_id: BlockId) -> TermInstId {
        // add predecessors whenever we jump or branch somewhere
        match terminator {
            TerminatorInst::Branch { span: _, cond: _, true_block, false_block } => {
                self.get_block_mut(block_id).successors.insert(true_block);
                self.get_block_mut(block_id).successors.insert(false_block);
                self.get_block_mut(true_block).predecessors.insert(block_id);
                self.get_block_mut(false_block).predecessors.insert(block_id);
            },
            TerminatorInst::Jump { span: _, target } => {
                self.get_block_mut(block_id).successors.insert(target);
                self.get_block_mut(target).predecessors.insert(block_id);
            },
            TerminatorInst::Return { .. } | TerminatorInst::Unreachable => {}, // no successors, nothing to update
        }
        let id = self.terminators.insert(terminator);
        self.get_block_mut(block_id).terminator = Some(id);
        id
    }
    // this is usually only used by other methods of IrArena, not directly by any pass
    // it shouldn't ever become a public method (usually you need to replace a terminator)
    fn remove_terminator_for(&mut self, block_id: BlockId) {
        if let Some(block) = self.try_get_block_mut(block_id) {
            if let Some(old_term_id) = block.terminator {
                // reset successors in case we are overwriting an old terminator
                block.successors.clear();
                // we also make sure we keep track of predecessors correctly
                match *self.get_term(old_term_id) {
                    TerminatorInst::Branch { true_block, false_block, .. } => {
                        self.get_block_mut(true_block).predecessors.remove(&block_id);
                        self.get_block_mut(false_block).predecessors.remove(&block_id);
                    },
                    TerminatorInst::Jump { target, .. } => {
                        self.get_block_mut(target).predecessors.remove(&block_id);
                    },
                    TerminatorInst::Return { .. } | TerminatorInst::Unreachable => {},
                }
                // in case we already had a terminator, this method is gonna overwrite the old one.
                // knowing that, its better to just remove that terminator, since it will be completely dead.
                self.terminators.remove(old_term_id);
            }
        }
    }
    pub fn alloc_block(&mut self, name: &str, func_id: IrFuncId) -> BlockId {
        let block_id = self.blocks.insert_with_key(|key| BasicBlock {
            parent_function: func_id,
            name: name.to_string() + "_" + &key.to_string(),
            predecessors: HashSet::new(), // always empty, filled by alloc_terminator_for
            successors: HashSet::new(),   // always empty, filled by alloc_terminator_for
            instructions: Vec::new(),
            terminator: None,
            display_name: name.to_string(),
        });
        self.func_id_to_blocks.entry(func_id).or_default().push(block_id);
        block_id
    }
    pub fn invalidate_block(&mut self, block_id: BlockId) {
        self.remove_terminator_for(block_id);
        if let Some(block) = self.blocks.remove(block_id) {
            // invalidating a block also invalidates all the instructions in that block
            for inst_id in block.instructions {
                self.invalidate_inst(inst_id);
            }
        }
    }
    pub fn block_is_valid(&self, block_id: BlockId) -> bool { self.blocks.contains_key(block_id) }
}

impl IrArena {
    pub fn pop_entire_hfs_stack(&mut self) -> Vec<InstId> {
        let temp = self.hfs_stack.clone();
        self.hfs_stack.clear();
        self.curr_block_stack.clear();
        temp
    }
    pub fn inst_name(&self, id: InstId) -> String {
        format!("{}{}", "%".custom_color(CustomColor::new(136, 151, 182)), format!("{}", id))
    }
    pub fn get_var(&self, id: GlobalIrVarId) -> &GlobalIrVarDeclaration { &self.global_vars[id] }
    pub fn get_func(&self, func_id: IrFuncId) -> &IrFunction { &self.functions[func_id] }
    pub fn get_func_mut(&mut self, func_id: IrFuncId) -> &mut IrFunction { &mut self.functions[func_id] }
    pub fn get_type(&self, id: TypeId) -> &IrType { &self.types[id.0] }
    pub fn get_inst_mut(&mut self, inst_id: InstId) -> &mut Instruction { &mut self.instructions[inst_id] }
    pub fn get_inst(&self, inst_id: InstId) -> &Instruction { &self.instructions[inst_id] }
    pub fn try_get_inst(&self, inst_id: InstId) -> Option<&Instruction> {
        // this is used by optimization passes and other analyses that don't know if we might've
        // deleted a held InstId, and will crash if there wasn't
        // therefore we simply skip over these if we receive a None
        self.instructions.get(inst_id)
    }
    pub fn get_term(&self, term_id: TermInstId) -> &TerminatorInst { &self.terminators[term_id] }
    pub fn get_block_term(&self, block_id: BlockId) -> &TerminatorInst {
        &self.terminators[self.blocks[block_id].terminator.expect("[internal error] found block with no terminator")]
    }
    pub fn get_term_mut(&mut self, term_id: TermInstId) -> &mut TerminatorInst { &mut self.terminators[term_id] }
    pub fn get_block(&self, block_id: BlockId) -> &BasicBlock { &self.blocks[block_id] }
    pub fn get_predecessors(&self, block_id: BlockId) -> &HashSet<BlockId> { &self.blocks[block_id].predecessors }
    pub fn get_block_mut(&mut self, block_id: BlockId) -> &mut BasicBlock { &mut self.blocks[block_id] }
    pub fn try_get_block(&self, block_id: BlockId) -> Option<&BasicBlock> { self.blocks.get(block_id) }
    pub fn try_get_block_mut(&mut self, block_id: BlockId) -> Option<&mut BasicBlock> { self.blocks.get_mut(block_id) }

    pub fn get_blocks_in(&self, func_id: IrFuncId) -> Vec<BlockId> {
        self.func_id_to_blocks.get(&func_id).map_or(vec![], |v| v.clone())
    }

    pub fn compare_stacks(
        &mut self,
        stack1: &Vec<InstId>,
        stack2: &Vec<InstId>,
        span: Span,
    ) -> Result<(), Box<dyn CompileError>> {
        let expected_count = stack1.len();
        let actual_count = stack2.len();
        if expected_count != actual_count {
            return ir_lowerer_error!(
                IrLowererErrorKind::MismatchingStackDepths(expected_count, actual_count),
                &self,
                None,
                span
            );
        }

        for (inst_id1, inst_id2) in stack1.iter().zip(stack2.iter()) {
            let type_id1 = self.get_type_id_of_inst(*inst_id1)?;
            let type_id2 = self.get_type_id_of_inst(*inst_id2)?;
            self.compare_types(type_id1, type_id2, vec![span])?;
        }
        Ok(())
    }

    pub fn compare_types(&self, type1: TypeId, type2: TypeId, spans: Vec<Span>) -> Result<(), Box<dyn CompileError>> {
        let actual_type = self.get_type(type1);
        let expected_type = self.get_type(type2);
        let span = merge_spans(spans);

        match (actual_type, expected_type) {
            (
                IrType::Tuple { type_ids: actual_types, ptr_count: actual_ptr_count },
                IrType::Tuple { type_ids: expected_types, ptr_count: expected_ptr_count },
            ) => {
                if actual_types.len() != expected_types.len() {
                    return ir_lowerer_error!(
                        IrLowererErrorKind::IncorrectTupleLength(expected_types.len(), actual_types.len()),
                        self,
                        None,
                        span
                    );
                }
                if actual_ptr_count != expected_ptr_count {
                    return ir_lowerer_error!(
                        IrLowererErrorKind::IncorrectPointerCount(*expected_ptr_count, *actual_ptr_count),
                        self,
                        None,
                        span
                    );
                }
                // Recursively validate each element
                for (&actual_elem_id, &expected_elem_id) in actual_types.iter().zip(expected_types.iter()) {
                    self.compare_types(actual_elem_id, expected_elem_id, vec![span])?;
                }
                Ok(())
            },
            (actual, expected) if actual == expected => Ok(()),
            (actual, expected) => ir_lowerer_error!(
                IrLowererErrorKind::MismatchingTypes(actual.get_repr(&self), expected.get_repr(&self)),
                self,
                None,
                span
            ),
        }
    }
}

impl IrArena {
    // Only expressions have types!
    pub fn get_type_of_operation(&mut self, op: &IrOperation) -> Result<TypeId, Box<dyn CompileError>> {
        match op {
            // Arithmetic operations: return the operand type
            IrOperation::Add(lhs, _rhs)
            | IrOperation::Sub(lhs, _rhs)
            | IrOperation::Mul(lhs, _rhs)
            | IrOperation::Div(lhs, _rhs)
            | IrOperation::Mod(lhs, _rhs) => {
                let lhs_type = self.get_type_id_of_inst(*lhs)?;
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
            | IrOperation::GreaterEqual(_, _) => Ok(IrType::new_bool(0).type_id()),
        }
    }

    pub fn get_type_id_of_inst(&mut self, inst_id: InstId) -> Result<TypeId, Box<dyn CompileError>> {
        match self.get_inst(inst_id).clone() {
            Instruction::Operation { span: _, op } => Ok(self.get_type_of_operation(&op)?),
            Instruction::Literal { span: _, literal } => match literal {
                Literal::Integer(_) => Ok(IrType::new_int(0).type_id()),
                Literal::Float(_) => Ok(IrType::new_float(0).type_id()),
                Literal::String(_) => Ok(IrType::new_string(0).type_id()),
                Literal::Bool(_) => Ok(IrType::new_bool(0).type_id()),
            },
            Instruction::Tuple { span, instructions } => {
                // Build tuple type from element types
                let mut element_types = Vec::new();
                for inst_id in instructions.clone() {
                    let elem_type = self.get_type_id_of_inst(inst_id)?;
                    element_types.push(elem_type);
                }

                let tuple_type = IrType::Tuple { type_ids: element_types, ptr_count: 0 };
                Ok(self.alloc_type(tuple_type, span.clone()))
            },
            Instruction::Parameter { span: _, index: _, type_id } => Ok(type_id),
            Instruction::FunctionCall { span: _, args: _, func_id, is_move: _, return_values: _ } =>
                Ok(self.get_func(func_id).return_type),
            Instruction::Phi { span: _, incoming } => Ok(self.get_type_id_of_inst(
                *incoming.values().next().expect("[internal error] found phi with no elements in type checking"),
            )?),
            Instruction::LoadElement { span: _, index: _, tuple: _ } => todo!(),
            Instruction::ReturnValue { span: _, type_id } => Ok(type_id),
            Instruction::Load { span: _, address: _, type_id } => Ok(type_id),
            Instruction::Store { span: _, address: _, value } => Ok(self.get_type_id_of_inst(value)?),
            Instruction::Alloca { span: _, type_id: _ } => {
                // implement this later
                panic!("[internal error] asked for the type of an alloca instruction but i don't see why this would happen")
            },
            Instruction::GlobalAlloca(_) => todo!(),
        }
    }
    pub fn get_type_of_var(&self, var_id: GlobalIrVarId) -> &IrType { self.get_type(self.get_var(var_id).hfs_type) }
    pub fn get_type_of_func(&self, func_id: IrFuncId) -> &IrType { self.get_type(self.get_func(func_id).return_type) }

    pub fn get_type_of_inst(&mut self, inst_id: InstId) -> Result<&IrType, Box<dyn CompileError>> {
        let type_id = self.get_type_id_of_inst(inst_id)?;
        Ok(self.get_type(type_id))
    }

    // returns itself with the pointer count reduced by one
    pub fn reduce_type_ptr_count(&mut self, type_id: TypeId, span: Span) -> TypeId {
        let hfs_type = match self.get_type(type_id) {
            IrType::Int { ptr_count } => IrType::Int { ptr_count: ptr_count - 1 },
            IrType::String { ptr_count } => IrType::String { ptr_count: ptr_count - 1 },
            IrType::Bool { ptr_count } => IrType::Bool { ptr_count: ptr_count - 1 },
            IrType::Float { ptr_count } => IrType::Float { ptr_count: ptr_count - 1 },
            IrType::Tuple { ptr_count, type_ids } => IrType::Tuple { ptr_count: ptr_count - 1, type_ids: type_ids.clone() },
            IrType::Array { hfs_type: _, length: _, ptr_count: _ } => todo!(),
        };
        self.alloc_type(hfs_type, span)
    }

    // these functions are only used for printing so we don't have to pass a mutable reference to an arena everywhere
    pub fn get_type_of_operation_no_alloc(&self, op: &IrOperation) -> Option<TypeId> {
        match op {
            IrOperation::Add(lhs, _rhs)
            | IrOperation::Sub(lhs, _rhs)
            | IrOperation::Mul(lhs, _rhs)
            | IrOperation::Div(lhs, _rhs)
            | IrOperation::Mod(lhs, _rhs) => self.get_type_id_of_inst_no_alloc(*lhs),
            IrOperation::Or(_, _)
            | IrOperation::And(_, _)
            | IrOperation::Not(_)
            | IrOperation::Equal(_, _)
            | IrOperation::NotEqual(_, _)
            | IrOperation::Less(_, _)
            | IrOperation::LessEqual(_, _)
            | IrOperation::Greater(_, _)
            | IrOperation::GreaterEqual(_, _) => Some(IrType::new_bool(0).type_id()),
        }
    }

    pub fn get_type_id_of_inst_no_alloc(&self, inst_id: InstId) -> Option<TypeId> {
        match self.get_inst(inst_id) {
            Instruction::Operation { op, .. } => self.get_type_of_operation_no_alloc(op),
            Instruction::Literal { literal, .. } => match literal {
                Literal::Integer(_) => Some(IrType::new_int(0).type_id()),
                Literal::Float(_) => Some(IrType::new_float(0).type_id()),
                Literal::String(_) => Some(IrType::new_string(0).type_id()),
                Literal::Bool(_) => Some(IrType::new_bool(0).type_id()),
            },
            Instruction::Tuple { instructions, .. } => {
                let element_types: Option<Vec<TypeId>> =
                    instructions.iter().map(|id| self.get_type_id_of_inst_no_alloc(*id)).collect();
                let element_types = element_types?;
                self.types.iter().enumerate().find_map(|(i, t)| {
                    if let IrType::Tuple { type_ids, .. } = t {
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
