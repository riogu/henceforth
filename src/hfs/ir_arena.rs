use std::{collections::HashMap, fmt::Debug, rc::Rc};

use colored::{Colorize, CustomColor};
use slotmap::SlotMap;

use crate::{
    cfg_analyzer_error,
    hfs::{
        BasicBlock, BlockId, GlobalIrVarDeclaration, GlobalIrVarId, InstId, Instruction, IrFuncId, IrFunction, SourceInfo,
        TermInstId, TerminatorInst,
        ast::*,
        cfg_analyzer_errors::CfgAnalyzerErrorKind,
        error::{CompileError, DiagnosticInfo},
    },
};

#[derive(Debug, Default)]
pub struct IrContext {
    pub curr_function: IrFuncId,
    pub curr_insert_block: BlockId,
}

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
    pub types: Vec<Type>,

    pub func_id_map: HashMap<FuncId, IrFuncId>,
    // has both local alloca and global alloca instructions
    pub var_id_to_alloca_map: HashMap<VarId, InstId>,

    pub type_source_infos: Vec<SourceInfo>,
    type_cache: HashMap<Type, TypeId>,

    pub hfs_stack: Vec<InstId>,
    // this is reset whenever we add a terminator instruction to the current block
    // its used to keep track of each merging stack
    pub curr_block_stack: Vec<InstId>,

    pub ir_context: IrContext,
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
        arena.alloc_type_uncached(Type::new_int(0), SourceInfo::new(0, 0, 0));
        arena.alloc_type_uncached(Type::new_float(0), SourceInfo::new(0, 0, 0));
        arena.alloc_type_uncached(Type::new_bool(0), SourceInfo::new(0, 0, 0));
        arena.alloc_type_uncached(Type::new_string(0), SourceInfo::new(0, 0, 0));
        arena
    }

    // Stack methods (manage the hfs stack for operations)
    pub fn pop_or_error(&mut self, source_infos: Vec<SourceInfo>, ast_arena: &AstArena) -> Result<InstId, Box<dyn CompileError>> {
        match self.pop_hfs_stack() {
            Some(id) => Ok(id),
            None => cfg_analyzer_error!(CfgAnalyzerErrorKind::StackUnderflow, &*self, Some(ast_arena), source_infos),
        }
    }
    pub fn last_or_error(
        &mut self,
        source_infos: Vec<SourceInfo>,
        ast_arena: &AstArena,
    ) -> Result<InstId, Box<dyn CompileError>> {
        match self.hfs_stack.last() {
            Some(id) => Ok(*id),
            None => cfg_analyzer_error!(CfgAnalyzerErrorKind::ExpectedItemOnStack, &*self, Some(ast_arena), source_infos),
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

    pub fn alloc_type_uncached(&mut self, hfs_type: Type, source_info: SourceInfo) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type.clone());
        self.type_source_infos.push(source_info);
        id
    }
    pub fn alloc_type(&mut self, hfs_type: Type, source_info: SourceInfo) -> TypeId {
        // Check if this type already exists
        if let Some(&existing_id) = self.type_cache.get(&hfs_type) {
            return existing_id;
        } // If not, allocate it
        self.alloc_type_uncached(hfs_type, source_info)
    }
    pub fn alloc_function(&mut self, func: IrFunction, old_id: FuncId) -> IrFuncId {
        let id = self.functions.insert(func);
        self.func_id_map.insert(old_id, id);
        self.ir_context.curr_function = id;
        id
    }

    pub fn alloc_inst_for(&mut self, inst: Instruction, block_id: BlockId) -> InstId {
        if matches!(inst, Instruction::Alloca { .. }) {
        } else if matches!(inst, Instruction::GlobalAlloca(..)) {
            panic!(
                "[internal error] please use alloc_global_var and alloc_local_var instead of alloca_inst_for when creating \
                 alloca instructions"
            )
        }
        let id = self.instructions.insert(inst);
        self.get_block_mut(block_id).instructions.push(id);
        id
    }

    pub fn alloc_local_var(&mut self, inst: Instruction, block_id: BlockId, var_id: VarId) -> InstId {
        // produces an alloca
        let id = self.instructions.insert(inst);
        self.var_id_to_alloca_map.insert(var_id, id);
        self.get_block_mut(block_id).instructions.push(id);
        id
    }
    pub fn alloc_global_var(&mut self, var: GlobalIrVarDeclaration, old_var_id: VarId) -> GlobalIrVarId {
        let global_var_id = self.global_vars.insert(var);

        let inst_id = self.instructions.insert(Instruction::GlobalAlloca(global_var_id));
        self.var_id_to_alloca_map.insert(old_var_id, inst_id);

        global_var_id
    }

    pub fn alloc_terminator_for(
        &mut self,
        terminator: TerminatorInst,
        block_id: BlockId,
    ) -> Result<TermInstId, Box<dyn CompileError>> {
        // add predecessors whenever we jump or branch somewhere
        match terminator {
            TerminatorInst::Branch { source_info: _, cond: _, true_block, false_block } => {
                self.get_block_mut(block_id).successors.push(true_block);
                self.get_block_mut(block_id).successors.push(false_block);
                self.get_block_mut(true_block).predecessors.push(block_id);
                self.get_block_mut(false_block).predecessors.push(block_id);
            },
            TerminatorInst::Jump(_, jump_to_id) => {
                self.get_block_mut(block_id).successors.push(jump_to_id);
                self.get_block_mut(jump_to_id).predecessors.push(block_id);
            },
            TerminatorInst::Return(..) | TerminatorInst::Unreachable => {}, // no successors, nothing to update
        }
        let id = self.terminators.insert(terminator);
        self.get_block_mut(block_id).terminator = Some(id);
        Ok(id)
    }
    pub fn alloc_block(&mut self, name: &str) -> BlockId {
        self.blocks.insert_with_key(|key| BasicBlock {
            parent_function: self.ir_context.curr_function,
            name: name.to_string() + "_" + &key.to_string(),
            predecessors: Vec::new(), // always empty, filled by alloc_terminator_for
            successors: Vec::new(),   // always empty, filled by alloc_terminator_for
            instructions: Vec::new(),
            terminator: None,
        })
    }
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
    pub fn get_func(&self, id: IrFuncId) -> &IrFunction { &self.functions[id] }
    pub fn get_type(&self, id: TypeId) -> &Type { &self.types[id.0] }
    pub fn get_instruction_mut(&mut self, id: InstId) -> &mut Instruction { &mut self.instructions[id] }
    pub fn get_instruction(&self, id: InstId) -> &Instruction { &self.instructions[id] }
    pub fn try_get_instruction(&self, id: InstId) -> Option<&Instruction> {
        // this is used by optimization passes and other analyses that don't know if we might've
        // deleted a held InstId, and will crash if there wasn't
        // therefore we simply skip over these if we receive a None
        self.instructions.get(id)
    }
    pub fn remove_instruction(&mut self, id: InstId) -> Option<Instruction> { self.instructions.remove(id) }

    pub fn get_terminator_instruction(&self, id: TermInstId) -> &TerminatorInst { &self.terminators[id] }
    pub fn get_block(&self, id: BlockId) -> &BasicBlock { &self.blocks[id] }
    pub fn get_block_mut(&mut self, id: BlockId) -> &mut BasicBlock { &mut self.blocks[id] }
    pub fn try_get_block(&self, id: BlockId) -> Option<&BasicBlock> { self.blocks.get(id) }

    pub fn compare_stacks(
        &mut self,
        stack1: &Vec<InstId>,
        stack2: &Vec<InstId>,
        source_infos: Vec<SourceInfo>,
    ) -> Result<(), Box<dyn CompileError>> {
        let expected_count = stack1.len();
        let actual_count = stack2.len();
        if expected_count != actual_count {
            return cfg_analyzer_error!(
                CfgAnalyzerErrorKind::MismatchingStackDepths(expected_count, actual_count),
                &self,
                None,
                source_infos
            );
        }

        for (inst_id1, inst_id2) in stack1.iter().zip(stack2.iter()) {
            let type_id1 = self.get_type_id_of_inst(*inst_id1)?;
            let type_id2 = self.get_type_id_of_inst(*inst_id2)?;
            self.compare_types(type_id1, type_id2, source_infos.clone())?;
        }
        Ok(())
    }

    pub fn compare_types(
        &self,
        type1: TypeId,
        type2: TypeId,
        source_infos: Vec<SourceInfo>,
    ) -> Result<(), Box<dyn CompileError>> {
        let actual_type = self.get_type(type1);
        let expected_type = self.get_type(type2);

        match (actual_type, expected_type) {
            (
                Type::Tuple { type_ids: actual_types, ptr_count: actual_ptr_count },
                Type::Tuple { type_ids: expected_types, ptr_count: expected_ptr_count },
            ) => {
                if actual_types.len() != expected_types.len() {
                    return cfg_analyzer_error!(
                        CfgAnalyzerErrorKind::IncorrectTupleLength(expected_types.len(), actual_types.len()),
                        self,
                        None,
                        source_infos
                    );
                }
                if actual_ptr_count != expected_ptr_count {
                    return cfg_analyzer_error!(
                        CfgAnalyzerErrorKind::IncorrectPointerCount(*expected_ptr_count, *actual_ptr_count),
                        self,
                        None,
                        source_infos
                    );
                }
                // Recursively validate each element
                for (i, (&actual_elem_id, &expected_elem_id)) in actual_types.iter().zip(expected_types.iter()).enumerate() {
                    let elem_source_info = match source_infos.get(i) {
                        Some(source_info) => source_info.clone(),
                        None => panic!("[internal error] wrong number of source infos passed"),
                    };
                    self.compare_types(actual_elem_id, expected_elem_id, vec![elem_source_info])?;
                }
                Ok(())
            },
            (actual, expected) if actual == expected => Ok(()),
            (actual, expected) => cfg_analyzer_error!(
                CfgAnalyzerErrorKind::MismatchingTypes(actual.clone(), expected.clone()),
                self,
                None,
                source_infos
            ),
        }
    }
}

// ============================================================================
// DefUseInfo related code
// used by the Optimizer
// ============================================================================
pub struct DefUseInfo {
    pub users: HashMap<InstId, Vec<InstId>>,
}
impl IrArena {
    // link a definition to all its uses
    pub fn compute(&self) { todo!() }
    pub fn users_of(&self, id: InstId) -> &[InstId] { todo!() }
    pub fn replace_all_uses_with(&mut self, old_id: InstId, new_id: InstId) {}
}
