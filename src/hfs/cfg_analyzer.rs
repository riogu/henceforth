use std::{
    any::Any,
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
};

use colored::{Colorize, CustomColor};

use crate::{
    cfg_analyzer_error,
    hfs::{
        self, BasicBlock, BlockId, CfgFunction, CfgOperation, CfgPrintable, CfgTopLevelId, GlobalIrVarDeclaration, GlobalIrVarId,
        InstId, Instruction, IrFuncId, Literal, PRIMITIVE_TYPE_COUNT, SourceInfo, TermInstId, TerminatorInst, Token, TokenKind,
        ast::*,
        cfg_analyzer_errors::{CfgAnalyzerError, CfgAnalyzerErrorKind},
        error::{CompileError, DiagnosticInfo},
    },
};

// here is where youll create the CFG pass and the new IR generation

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BlockContext {
    continue_to_block: Option<BlockId>,
    break_to_block: Option<BlockId>,
    end_block: Option<BlockId>,
    prev_stack_change: Vec<InstId>,
    stack_snapshots: Vec<(BlockId, Vec<InstId>)>,
    // its useful for stuff like break statements
}

#[derive(Debug, Default)]
pub struct CfgContext {
    pub curr_function: IrFuncId,
    pub curr_insert_block: BlockId,
    // we use this to track what the stack is for each construct
    // such as if statements. we build it up on the first branch, and then after that we compare
    // the new branches with the first one, for the same context
}

#[derive(Debug, Default)]
pub struct IrArena {
    // TODO: we also need to pass metadata into this new IR
    // which includes expression provenance and etc (for optimizations and annotations)
    // we should make an annotation system and convert into it
    pub global_vars: Vec<GlobalIrVarDeclaration>,
    pub functions: Vec<CfgFunction>,
    pub instructions: Vec<Instruction>,
    pub terminators: Vec<TerminatorInst>,
    pub blocks: Vec<BasicBlock>,
    pub types: Vec<Type>,

    pub func_id_map: HashMap<FuncId, IrFuncId>,
    // has both local alloca and global alloca instructions
    var_id_to_alloca_map: HashMap<VarId, InstId>,

    pub type_source_infos: Vec<SourceInfo>,
    type_cache: HashMap<Type, TypeId>,

    hfs_stack: Vec<InstId>,
    // this is reset whenever we add a terminator instruction to the current block
    // its used to keep track of each merging stack
    pub curr_block_stack: Vec<InstId>,

    pub cfg_context: CfgContext,
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
    fn push_to_hfs_stack(&mut self, inst: InstId) {
        self.hfs_stack.push(inst);
        // also pushes it to the context used for verifying equality between branches
        self.curr_block_stack.push(inst);
    }
    // use this instead of popping the stack directly because we keep track of other things
    fn pop_hfs_stack(&mut self) -> Option<InstId> {
        // also pushes it to the context used for verifying equality between branches
        self.curr_block_stack.pop();
        self.hfs_stack.pop()
    }

    fn alloc_type_uncached(&mut self, hfs_type: Type, source_info: SourceInfo) -> TypeId {
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
    fn alloc_function(&mut self, func: CfgFunction, old_id: FuncId) -> IrFuncId {
        let id = IrFuncId(self.functions.len());
        self.functions.push(func);
        self.func_id_map.insert(old_id, id);
        self.cfg_context.curr_function = id;
        id
    }

    fn alloc_stack_keyword_as_func(&mut self, func: CfgFunction, old_id: ExprId) -> IrFuncId {
        let id = IrFuncId(self.functions.len());
        self.functions.push(func);
        self.cfg_context.curr_function = id;
        id
    }

    fn alloc_inst_for(&mut self, inst: Instruction, block_id: BlockId) -> InstId {
        if matches!(inst, Instruction::Alloca { .. }) {
        } else if matches!(inst, Instruction::GlobalAlloca(..)) {
            panic!(
                "[internal error] please use alloc_global_var and alloc_local_var instead of alloca_inst_for when creating \
                 alloca instructions"
            )
        }
        let id = InstId(self.instructions.len());
        self.instructions.push(inst);
        self.get_block_mut(block_id).instructions.push(id);
        id
    }

    fn alloc_local_var(&mut self, inst: Instruction, block_id: BlockId, var_id: VarId) -> InstId {
        // produces an alloca
        let id = InstId(self.instructions.len());
        self.instructions.push(inst);
        self.var_id_to_alloca_map.insert(var_id, id);
        self.get_block_mut(block_id).instructions.push(id);
        id
    }
    pub fn alloc_global_var(&mut self, var: GlobalIrVarDeclaration, old_var_id: VarId) -> GlobalIrVarId {
        let global_var_id = GlobalIrVarId(self.global_vars.len());
        let inst_id = InstId(self.instructions.len());

        self.global_vars.push(var);

        self.instructions.push(Instruction::GlobalAlloca(global_var_id));
        self.var_id_to_alloca_map.insert(old_var_id, inst_id);

        global_var_id
    }

    pub fn alloc_terminator_for(
        &mut self,
        mut terminator: TerminatorInst,
        block_id: BlockId,
    ) -> Result<TermInstId, Box<dyn CompileError>> {
        // add predecessors whenever we jump or branch somewhere
        match terminator {
            TerminatorInst::Branch { ref source_info, cond, true_block, false_block } => {
                self.get_block_mut(true_block).predecessors.push(block_id);
                self.get_block_mut(false_block).predecessors.push(block_id);
            },
            TerminatorInst::Jump(ref source_info, jump_to_id) => {
                self.get_block_mut(jump_to_id).predecessors.push(block_id);
            },
            TerminatorInst::Return(..) | TerminatorInst::Unreachable => {}, // no successors, nothing to update
        }
        let id = TermInstId(self.terminators.len());
        self.terminators.push(terminator);
        self.get_block_mut(block_id).terminator = Some(id);
        Ok(id)
    }
    pub fn alloc_block(&mut self, name: &str) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(BasicBlock {
            parent_function: self.cfg_context.curr_function,
            name: name.to_string() + "_" + &id.0.to_string(),
            predecessors: Vec::new(), // always empty, filled by alloc_terminator_for
            instructions: Vec::new(),
            terminator: None,
        });
        id
    }

    fn get_func_by_name(&self, name: String) -> Option<IrFuncId> {
        let func = self.functions.iter().position(|f| f.name == name);
        match func {
            Some(id) => Some(IrFuncId(id)),
            None => None,
        }
    }
}

impl IrArena {
    pub fn pop_entire_hfs_stack(&mut self) -> Vec<InstId> {
        let temp = self.hfs_stack.clone();
        self.hfs_stack.clear();
        temp
    }
    pub fn inst_name(&self, id: InstId) -> String {
        format!("{}{}", "%".custom_color(CustomColor::new(136, 151, 182)), format!("{}", id.0))
    }
    pub fn get_var(&self, id: GlobalIrVarId) -> &GlobalIrVarDeclaration {
        &self.global_vars[id.0]
    }
    pub fn get_func(&self, id: IrFuncId) -> &CfgFunction {
        &self.functions[id.0]
    }
    pub fn get_type(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }
    pub fn get_instruction(&self, id: InstId) -> &Instruction {
        &self.instructions[id.0]
    }
    pub fn get_instruction_mut(&mut self, id: InstId) -> &mut Instruction {
        &mut self.instructions[id.0]
    }
    pub fn get_terminator_instruction(&self, id: TermInstId) -> &TerminatorInst {
        &self.terminators[id.0]
    }
    pub fn get_block(&self, id: BlockId) -> &BasicBlock {
        &self.blocks[id.0]
    }
    pub fn get_block_mut(&mut self, id: BlockId) -> &mut BasicBlock {
        &mut self.blocks[id.0]
    }

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
                    self.compare_types(actual_elem_id, expected_elem_id, vec![elem_source_info]);
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

#[derive(Debug)]
pub struct CfgAnalyzer {
    pub ast_arena: AstArena,
    pub arena: IrArena,

    // this is used to get rid of duplication issues while lowering stack blocks
    pub lowered_expr_cache: HashMap<ExprId, InstId>,
}

impl CfgAnalyzer {
    pub fn new(ast_arena: AstArena, diagnostic_info: Rc<DiagnosticInfo>) -> Self {
        let mut arena = IrArena::new(diagnostic_info);
        arena.types.extend_from_slice(&ast_arena.types[PRIMITIVE_TYPE_COUNT..]);
        Self { ast_arena, arena, lowered_expr_cache: HashMap::new() }
    }

    pub fn lower_to_mir(
        top_level: Vec<TopLevelId>,
        ast_arena: AstArena,
        diagnostic_info: Rc<DiagnosticInfo>,
    ) -> Result<(Vec<CfgTopLevelId>, IrArena), Box<dyn CompileError>> {
        let mut cfg_analyzer = CfgAnalyzer::new(ast_arena, diagnostic_info);
        let analyzed_top_level = cfg_analyzer.lower_top_level(top_level)?;
        // cfg_analyzer.print_hfs_mir(analyzed_top_level);
        Ok((analyzed_top_level, cfg_analyzer.arena))
    }

    fn lower_top_level(&mut self, top_level: Vec<TopLevelId>) -> Result<Vec<CfgTopLevelId>, Box<dyn CompileError>> {
        let mut analyzed_nodes = Vec::<CfgTopLevelId>::new();
        for node in top_level.clone() {
            let new_node = match node {
                TopLevelId::VariableDecl(id) => CfgTopLevelId::GlobalVarDecl(self.lower_global_variable_declaration(id)),
                TopLevelId::FunctionDecl(id) => CfgTopLevelId::FunctionDecl(self.lower_function_declaration(id)?),
                TopLevelId::Statement(id) =>
                    return cfg_analyzer_error!(
                        CfgAnalyzerErrorKind::NoStatementsInGlobalScope,
                        &self.arena,
                        Some(&self.ast_arena),
                        vec![self.ast_arena.get_stmt_token(id).source_info.clone()]
                    ),
            };
            analyzed_nodes.push(new_node);
        }
        Ok(analyzed_nodes)
    }

    fn lower_global_variable_declaration(&mut self, id: VarId) -> GlobalIrVarId {
        // we don't do anything here at all right now
        // maybe if we add assignments to declarations we might want to in the future but for now this doesn't do anything
        let var = self.ast_arena.get_var(id);
        self.arena.alloc_global_var(
            GlobalIrVarDeclaration {
                source_info: self.ast_arena.get_var_token(id).source_info.clone(),
                name: var.name.clone(),
                hfs_type: var.hfs_type,
            },
            id,
        )
    }
    fn lower_local_variable_declaration(&mut self, id: VarId) -> InstId {
        // Note that all variables are allocated at the function entry point to make mem2reg simpler
        // since it only works with allocas at the function entry
        let var = self.ast_arena.get_var(id);
        let entry_block = self.arena.get_func(self.arena.cfg_context.curr_function).entry_block;
        let inst_id = self.arena.alloc_local_var(
            Instruction::Alloca { source_info: self.ast_arena.get_var_token(id).source_info.clone(), type_id: var.hfs_type },
            entry_block,
            id,
        );
        inst_id
    }

    fn lower_stack_keyword_declaration(&mut self, id: ExprId) -> Result<IrFuncId, Box<dyn CompileError>> {
        let (kw_name, kw_param_type, kw_return_type, kw_parameter_exprs) = match self.ast_arena.get_expr(id) {
            Expression::StackKeyword(kw) =>
                (kw.name.clone(), kw.param_type.clone(), kw.return_type.clone(), kw.parameter_exprs.clone()),
            _ => panic!("[internal error] stack keyword id expected"),
        };
        let source_info = self.ast_arena.get_expr_token(id).source_info.clone();

        // check if declaration already exists (since stack keyword declarations are generated on call)
        if let Some(id) = self.arena.get_func_by_name(kw_name.clone()) {
            return Ok(id);
        }

        let entry_block = self.arena.alloc_block("start");
        self.arena.cfg_context.curr_insert_block = entry_block;

        let mut parameter_exprs = Vec::<InstId>::new();
        for param_expr_id in kw_parameter_exprs.iter() {
            let param_inst = self.lower_expr(*param_expr_id)?;
            parameter_exprs.push(param_inst);
            self.arena.push_to_hfs_stack(param_inst);
        }

        let curr_block_context = BlockContext {
            continue_to_block: None,
            break_to_block: None,
            end_block: None,
            prev_stack_change: vec![],
            stack_snapshots: vec![],
        };
        if self.arena.get_block(self.arena.cfg_context.curr_insert_block).terminator.is_none() {
            let instructions = self.arena.pop_entire_hfs_stack();
            let return_tuple = self.arena.alloc_inst_for(
                Instruction::Tuple { source_info: source_info.clone(), instructions },
                self.arena.cfg_context.curr_insert_block,
            );
            let term = self.arena.alloc_terminator_for(
                TerminatorInst::Return(source_info.clone(), return_tuple),
                self.arena.cfg_context.curr_insert_block,
            );
        }

        let cfg_function = CfgFunction {
            source_info,
            name: kw_name,
            param_type: kw_param_type,
            return_type: kw_return_type,
            parameter_insts: parameter_exprs,
            entry_block,
        };

        self.arena.pop_entire_hfs_stack(); // context should be reset after each function!
        Ok(self.arena.alloc_stack_keyword_as_func(cfg_function, id))
    }

    fn lower_function_declaration(&mut self, id: FuncId) -> Result<IrFuncId, Box<dyn CompileError>> {
        let func_decl = self.ast_arena.get_func(id).clone();
        let source_info = self.ast_arena.get_function_token(id).source_info.clone();

        let entry_block = self.arena.alloc_block("start"); // create before analyzing the parameters
        self.arena.cfg_context.curr_insert_block = entry_block;

        let mut parameter_exprs = Vec::<InstId>::new();
        for param_expr_id in func_decl.parameter_exprs {
            let param_inst = self.lower_expr(param_expr_id)?;
            parameter_exprs.push(param_inst);
            self.arena.push_to_hfs_stack(param_inst);
        }
        let cfg_function = CfgFunction {
            source_info: source_info.clone(),
            name: func_decl.name,
            param_type: func_decl.param_type,
            return_type: func_decl.return_type,
            parameter_insts: parameter_exprs,
            entry_block,
        };
        let func_id = self.arena.alloc_function(cfg_function, id);
        // note that this needs to be allocated before we lower the body so we can access the
        // current function definition (ex: to put allocas at the start)

        let curr_block_context = BlockContext {
            continue_to_block: None,
            break_to_block: None,
            end_block: None,
            prev_stack_change: vec![],
            stack_snapshots: vec![],
        };
        self.lower_stmt(func_decl.body, curr_block_context);
        if self.arena.get_block(self.arena.cfg_context.curr_insert_block).terminator.is_none() {
            // add implicit return at the end of the function if the block is unfinished
            let instructions = self.arena.pop_entire_hfs_stack();
            let return_tuple = self.arena.alloc_inst_for(
                Instruction::Tuple { source_info: source_info.clone(), instructions },
                self.arena.cfg_context.curr_insert_block,
            );
            let term = self.arena.alloc_terminator_for(
                TerminatorInst::Return(source_info.clone(), return_tuple),
                self.arena.cfg_context.curr_insert_block,
            );
        }
        self.arena.hfs_stack.clear(); // context should be reset after each function!
        Ok(func_id)
    }
    // TODO: dont forget about issues with dead code elimination. if we have code after
    // a return; or something, we gotta be careful to eliminate it at some point
    // otherwise we might have issues. maybe we should do that in StackAnalyzer instead?
    // watch out for accidentally overwriting the old terminator if we run this code without dead code elimination
    // basically, this code expects dead code elimination to have occurred BEFORE
    // FIXME: implement a small dead code elimination on the AST before cfg_analyzer

    /* example of CFG blocks that cover many of the cases of the code below:
      start_function:
          branch 1 < 2.0, if_body_0, else_if_cond_0;
          if_body_0:
              jump if_end_0;
          else_if_cond_0:
              branch -420 < 5, else_if_body_0, else_if_cond_1;
              else_if_body_0:
                  jump if_end_0;
          else_if_cond_1:
              stack becomes:
              branch -3 < 5, else_if_body_1, else_body_0;
              else_if_body_1:
                  jump if_end_0;
          else_body_0:
              jump if_end_0;
          if_end_0:
              jump end_function;

      end_function:
          return;
    */

    fn lower_if_condition(
        &mut self,
        cond_stack_block: StmtId,
        body: StmtId,
        curr_block_context: &BlockContext,
    ) -> Result<(InstId, BlockId, BlockId), Box<dyn CompileError>> {
        //
        let block_before_if = self.arena.cfg_context.curr_insert_block;

        self.lower_stmt(cond_stack_block, curr_block_context.clone());

        let if_body_block = self.arena.alloc_block("if_body");
        self.arena.cfg_context.curr_insert_block = if_body_block;

        // condition isnt included in the stack depth count
        let cond = match self.arena.pop_hfs_stack() {
            Some(cond) => cond,
            None =>
                return cfg_analyzer_error!(CfgAnalyzerErrorKind::StackUnderflow, &self.arena, Some(&self.ast_arena), vec![
                    self.ast_arena.get_stmt_token(body).source_info.clone()
                ]),
        };
        let cond_type = self.arena.get_type_id_of_inst(cond)?;
        self.arena
            .compare_types(cond_type, self.arena.bool_type(), vec![self.arena.get_instruction(cond).get_source_info()])?;

        Ok((cond, block_before_if, if_body_block))
    }
    fn lower_if_body(
        &mut self,
        body: StmtId,
        curr_block_context: &BlockContext,
        new_if_context: bool,
    ) -> Result<(BlockId, Vec<InstId>), Box<dyn CompileError>> {
        self.lower_stmt(body, curr_block_context.clone()); // pay attention to this call (we manage state around it)

        let stack_after_body = self.arena.hfs_stack.clone();

        let if_end_block = if new_if_context {
            self.arena.alloc_block("if_end")
        } else {
            // validate that we aren't getting a different stack depth
            self.arena.compare_stacks(
                &stack_after_body,
                &curr_block_context.prev_stack_change,
                stack_after_body.iter().map(|inst| self.arena.get_instruction(*inst).get_source_info()).collect(),
            )?;
            curr_block_context.end_block.expect("[internal error] forgot to set exit block for the current if stmt")
        };

        Ok((if_end_block, stack_after_body))
    }

    fn generate_merge_phis(
        &mut self,
        if_end_block: BlockId,
        stack_snapshots: &[(BlockId, Vec<InstId>)],
        source_info: &SourceInfo,
    ) {
        // to solve stack balancing, we keep track of the entire stack across branches
        // then, we compare what changed from one branch to the other
        // since we still have the stack from before we branched off, we need to skip elements that
        // are repeated across branches. by doing this, we have a simple and consistent logic for
        // creating phis that solve the disagreements between control flow paths.

        self.arena.hfs_stack.clear(); // reset the stack and create it again

        let result_len = stack_snapshots[0].1.len(); // all branchs must be the same length
        for stack_idx in 0..result_len {
            // take all stacks and group each element with each other into a phi
            // for ex: we group every 1st pushed value together for each branch
            let values: Vec<InstId> = stack_snapshots.iter().map(|(_, stack)| stack[stack_idx]).collect();
            if values.iter().all(|v| *v == values[0]) {
                // if the stack is the same across branches, then we shouldn't touch anything
                self.arena.hfs_stack.push(values[0]);
            } else {
                let mut incoming = HashMap::<BlockId, InstId>::new();
                for (block_id, snapshot) in stack_snapshots {
                    incoming.insert(*block_id, snapshot[stack_idx]);
                }
                let phi =
                    self.arena.alloc_inst_for(Instruction::Phi { source_info: source_info.clone(), incoming }, if_end_block);
                self.arena.hfs_stack.push(phi);
            }
        }
        self.arena.cfg_context.curr_insert_block = if_end_block;
    }

    fn lower_stmt(&mut self, id: StmtId, curr_block_context: BlockContext) -> Result<(), Box<dyn CompileError>> {
        let source_info = self.ast_arena.get_stmt_token(id).source_info.clone();

        match self.ast_arena.get_stmt(id).clone() {
            Statement::Else(stmt_id) => self.lower_stmt(stmt_id, curr_block_context),
            if_stmt @ Statement::ElseIf { cond_stack_block, body, else_stmt }
            | if_stmt @ Statement::If { cond_stack_block, body, else_stmt } => {
                // means we are starting a new chain of ifs
                let mut stack_snapshots = curr_block_context.stack_snapshots.clone();

                let (cond, block_before_if, if_body_block) =
                    self.lower_if_condition(cond_stack_block, body, &curr_block_context)?;

                let stack_before_branches = self.arena.hfs_stack.clone();

                let (if_end_block, stack_after_if_body) =
                    self.lower_if_body(body, &curr_block_context, matches!(if_stmt, Statement::If { .. }))?;
                stack_snapshots.push((self.arena.cfg_context.curr_insert_block, stack_after_if_body.clone()));

                if self.arena.get_block(self.arena.cfg_context.curr_insert_block).terminator.is_none() {
                    self.arena.alloc_terminator_for(
                        TerminatorInst::Jump(source_info.clone(), if_end_block),
                        self.arena.cfg_context.curr_insert_block,
                    );
                }
                // put the condition on the stack again for validating the other branches
                // (it wasnt consumed in this path)
                self.arena.push_to_hfs_stack(cond);

                if let Some(else_id) = else_stmt {
                    let else_stmt = self.ast_arena.get_stmt(else_id);

                    let name = if let Statement::ElseIf { .. } = else_stmt {
                        "else_if_cond".to_string()
                    } else {
                        "else_body".to_string()
                    };
                    match else_stmt {
                        Statement::Else(_) | Statement::ElseIf { .. } => {
                            let is_else = matches!(else_stmt, Statement::Else(_));

                            let else_body_block = self.arena.alloc_block(&name);

                            let if_branch_inst = self.arena.alloc_terminator_for(
                                TerminatorInst::Branch {
                                    source_info: source_info.clone(),
                                    cond,
                                    true_block: if_body_block,
                                    false_block: else_body_block,
                                },
                                block_before_if,
                            );

                            self.arena.cfg_context.curr_insert_block = else_body_block;

                            let curr_block_context = BlockContext {
                                continue_to_block: curr_block_context.continue_to_block,
                                break_to_block: curr_block_context.break_to_block,
                                end_block: Some(if_end_block),
                                prev_stack_change: stack_after_if_body.clone(),
                                stack_snapshots: stack_snapshots.clone(),
                            };

                            self.arena.hfs_stack = stack_before_branches.clone(); // restore stack before else
                            self.lower_stmt(else_id, curr_block_context); // pay attention to this call (we manage state around it)
                            let else_snapshot = self.arena.hfs_stack.clone();
                            stack_snapshots.push((self.arena.cfg_context.curr_insert_block, else_snapshot));

                            if self.arena.get_block(self.arena.cfg_context.curr_insert_block).terminator.is_none()
                            // if the 'else' branch already set curr_insert_block to if_end_block,
                            // we dont want the block to jump to itself
                            && self.arena.cfg_context.curr_insert_block != if_end_block
                            {
                                self.arena.alloc_terminator_for(
                                    TerminatorInst::Jump(source_info.clone(), if_end_block),
                                    self.arena.cfg_context.curr_insert_block,
                                );
                            }
                            if is_else {
                                self.generate_merge_phis(if_end_block, &stack_snapshots, &source_info);
                            }
                            Ok(())
                        },
                        otherstmt => panic!("can't have other statements in else statement, found '{:?}'", otherstmt),
                    }
                } else {
                    if self.arena.hfs_stack.len() != stack_before_branches.len() {
                        return cfg_analyzer_error!(
                            CfgAnalyzerErrorKind::ExpectedNetZeroStackEffectIfStmt(self.arena.hfs_stack.len()),
                            &self.arena,
                            Some(&self.ast_arena),
                            vec![self.ast_arena.get_stmt_token(body).source_info.clone()]
                        );
                    };
                    self.arena.alloc_terminator_for(
                        TerminatorInst::Branch { source_info, cond, true_block: if_body_block, false_block: if_end_block },
                        block_before_if,
                    );
                    Ok(())
                }
            },
            Statement::While { cond, body } => {
                /* jump while_cond_0;
                   while_cond_0:
                       branch example != 0, while_block_0, while_end_0;
                       while_block_0:
                           store example, example - 1;
                           jump while_cond_0;
                   while_end_0:
                       jump end;
                */
                let while_cond_block = self.arena.alloc_block("while_cond");
                // cant seal the condition right away
                let while_body_block = self.arena.alloc_block("while_body");
                let while_end_block = self.arena.alloc_block("while_end");

                self.arena.alloc_terminator_for(
                    TerminatorInst::Jump(source_info.clone(), while_cond_block),
                    self.arena.cfg_context.curr_insert_block,
                ); // finish the previous block with a jump

                //--------------------------------------------------------------------------
                // set up the context for lowering the while body
                self.arena.cfg_context.curr_insert_block = while_cond_block;
                let cond = self.lower_expr(cond)?;
                let cond_type = self.arena.get_type_id_of_inst(cond)?;
                self.arena
                    .compare_types(cond_type, self.arena.bool_type(), vec![self.arena.get_instruction(cond).get_source_info()])?;
                self.arena.alloc_terminator_for(
                    TerminatorInst::Branch {
                        source_info: source_info.clone(),
                        cond,
                        true_block: while_body_block,
                        false_block: while_end_block,
                    },
                    while_cond_block,
                );

                let curr_block_context = BlockContext {
                    continue_to_block: Some(while_cond_block),
                    break_to_block: Some(while_end_block),
                    end_block: None,
                    prev_stack_change: vec![],
                    stack_snapshots: vec![],
                };

                let stack_depth_before = self.arena.hfs_stack.len();
                self.arena.cfg_context.curr_insert_block = while_body_block;
                self.lower_stmt(body, curr_block_context);

                // Enforce stack balance
                let stack_depth_after = self.arena.hfs_stack.len();
                if stack_depth_before != stack_depth_after {
                    return cfg_analyzer_error!(
                        CfgAnalyzerErrorKind::ExpectedNetZeroStackEffectWhileLoop(stack_depth_after - stack_depth_before),
                        &self.arena,
                        Some(&self.ast_arena),
                        vec![self.ast_arena.get_stmt_token(body).source_info.clone()]
                    );
                }
                if self.arena.get_block(self.arena.cfg_context.curr_insert_block).terminator.is_none() {
                    self.arena.alloc_terminator_for(
                        TerminatorInst::Jump(source_info, while_cond_block),
                        self.arena.cfg_context.curr_insert_block,
                    );
                }
                // dont forget to put the context where it should be after we are done with the while loop
                self.arena.cfg_context.curr_insert_block = while_end_block;
                Ok(())
                //--------------------------------------------------------------------------
            },
            Statement::StackBlock(expr_ids) =>
            /* @(1 2 3)
               if @(1 2 <) {
                   let var: i32;
                   @(4 5) &= var; // @(1 2 3 4)
                   if (true) {
                       @(5 6) // @(1 2 3 4 5 6)
                   }
                   else {
                       @(5 6) // @(1 2 3 4 5 6)
                   }
               } else if @( 3 4 <) {
                   @(4 5 6) // @(1 2 3 4 5 6)
               }
            */
            {
                for expr_id in expr_ids {
                    // we add stack operations as standalone instructions in the current block (just like normal SSA)
                    // the stack semantics are validated but they act "like local variables"
                    let inst = self.lower_expr(expr_id)?;
                    self.arena.push_to_hfs_stack(inst);
                }
                Ok(())
            },
            Statement::BlockScope(top_level_ids, scope_kind) => {
                // eprintln!("BlockScope top_level_ids: {:?}", top_level_ids);
                for top_level_id in top_level_ids.clone() {
                    match top_level_id {
                        TopLevelId::VariableDecl(var_id) => {
                            self.lower_local_variable_declaration(var_id);
                        },
                        TopLevelId::FunctionDecl(func_id) => panic!("[internal error] local functions are not allowed"),
                        TopLevelId::Statement(stmt_id) => self.lower_stmt(stmt_id, curr_block_context.clone())?,
                    };
                }
                Ok(())
            },
            Statement::Return => {
                // note that we have already checked that the state of the stack is correct in
                // terms of size and types before each return statement.
                // what the cfg_analyzer does is also check all branches against each other.
                let return_tuple = self.arena.alloc_inst_for(
                    Instruction::Tuple { source_info: source_info.clone(), instructions: self.arena.hfs_stack.clone() },
                    self.arena.cfg_context.curr_insert_block,
                );
                let term = self.arena.alloc_terminator_for(
                    TerminatorInst::Return(source_info, return_tuple),
                    self.arena.cfg_context.curr_insert_block,
                );
                Ok(())
            },
            Statement::Break => {
                // a break always ends the current block and jumps somewhere else
                // (the exit of the current control flow construct)
                self.arena.alloc_terminator_for(
                    TerminatorInst::Jump(
                        source_info,
                        curr_block_context.break_to_block.expect("[internal error] found break outside of while context"),
                    ),
                    self.arena.cfg_context.curr_insert_block,
                );
                Ok(())
            },
            Statement::Continue => {
                // the entry_block is meant to be the block that we came from to start this current
                // context. its meant to allow the start of the next iteration
                self.arena.alloc_terminator_for(
                    TerminatorInst::Jump(
                        source_info,
                        curr_block_context.continue_to_block.expect("[internal error] found continue outside of while context"),
                    ),
                    self.arena.cfg_context.curr_insert_block,
                );
                Ok(())
            },
            Statement::Empty => {
                Ok(()) /* do nothing */
            },
            Statement::Assignment { identifier, is_move, deref_count } => {
                // @(213) &= var; // move assignment
                // @(213) := var; // copy assignment
                let inst_value = if is_move {
                    match self.arena.pop_hfs_stack() {
                        Some(val) => val,
                        None =>
                            return cfg_analyzer_error!(
                                CfgAnalyzerErrorKind::StackUnderflow,
                                &self.arena,
                                Some(&self.ast_arena),
                                vec![
                                    self.ast_arena.get_stmt_token(id).source_info.clone(),
                                    identifier.get_source_info(&self.ast_arena)
                                ]
                            ),
                    }
                } else {
                    match self.arena.hfs_stack.last() {
                        Some(val) => *val,
                        None =>
                            return cfg_analyzer_error!(
                                CfgAnalyzerErrorKind::ExpectedItemOnStack,
                                &self.arena,
                                Some(&self.ast_arena),
                                vec![
                                    self.ast_arena.get_stmt_token(id).source_info.clone(),
                                    identifier.get_source_info(&self.ast_arena)
                                ]
                            ),
                    }
                };
                let (mut address, type_id) = match identifier {
                    Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) =>
                        match self.arena.var_id_to_alloca_map.get(&var_id) {
                            Some(alloca_inst) => (*alloca_inst, self.ast_arena.get_var(var_id).hfs_type),
                            None => panic!("[internal error] forgot to alloca a variable before using it"),
                        },
                    Identifier::Function(func_id) => unreachable!("can't happen"),
                };
                // Chase the pointer chain: ptr^ is 1 deref, ptr^^ is 2, etc.
                if deref_count > 0 {
                    for _ in 0..deref_count {
                        let type_id = self.arena.reduce_type_ptr_count(type_id, source_info.clone());
                        address = self.arena.alloc_inst_for(
                            Instruction::Load { source_info: source_info.clone(), address, type_id },
                            self.arena.cfg_context.curr_insert_block,
                        );
                    }
                }
                self.arena.alloc_inst_for(
                    Instruction::Store { source_info: source_info.clone(), address, value: inst_value },
                    self.arena.cfg_context.curr_insert_block,
                );
                Ok(())
            },
            Statement::FunctionCall { arg_count, func_id, is_move, return_values } => {
                // @(213) &> func; // move call
                // @(213) :> func; // copy call
                let mut inst_args = Vec::<InstId>::new();
                for arg in 0..arg_count {
                    let arg_inst = match self.arena.hfs_stack.pop() {
                        Some(inst) => inst,
                        None =>
                            return cfg_analyzer_error!(
                                CfgAnalyzerErrorKind::StackUnderflow,
                                &self.arena,
                                Some(&self.ast_arena),
                                vec![
                                    self.ast_arena.get_stmt_token(id).source_info.clone(),
                                    self.ast_arena.get_function_token(func_id).source_info.clone(),
                                ]
                            ),
                    };
                    inst_args.push(arg_inst);
                }
                let func_id = match self.arena.func_id_map.get(&func_id) {
                    Some(func_id) => *func_id,
                    None => panic!("[internal error] tried making a function call before creating the associated IrFuncId"),
                };
                let mut new_return_vals = Vec::new();
                for val in return_values {
                    let source_info = self.ast_arena.get_expr_token(val).source_info.clone();
                    let Expression::ReturnValue(type_id) = self.ast_arena.get_expr(val) else {
                        panic!("[internal error] expected Expression::ReturnValue in return_values")
                    };
                    let retval = self.arena.alloc_inst_for(
                        Instruction::ReturnValue { source_info, type_id: *type_id },
                        self.arena.cfg_context.curr_insert_block,
                    );
                    self.arena.push_to_hfs_stack(retval); // keep the stack state correct
                    new_return_vals.push(retval);
                }
                self.arena.alloc_inst_for(
                    Instruction::FunctionCall { source_info, args: inst_args, func_id, is_move, return_values: new_return_vals },
                    self.arena.cfg_context.curr_insert_block,
                );
                Ok(())
            },
        }
    }
    fn lower_expr(&mut self, id: ExprId) -> Result<InstId, Box<dyn CompileError>> {
        if let Some(cached_inst_id) = self.lowered_expr_cache.get(&id) {
            // we used an expr cache to fix issues with duplication of expressions and not
            // correctly balancing the stack after building out expression trees
            // common example that causes issues:
            // @(1 2 3 4);
            // @(+ + + 2);
            // the stack should see at the end:
            // @(+ 2);
            // because these are the top level nodes that actually matter for stack balance
            // however this wont happen because exprid are duplicated.
            // to fix this, we use an expr_id cache to not recompute previous expressions
            // it is actually necessary for correctness, not just to remove wasted work.
            return Ok(*cached_inst_id);
        }

        let source_info = self.ast_arena.get_expr_token(id).source_info.clone();
        let inst_id = match self.ast_arena.get_expr(id).clone() {
            Expression::Operation(operation) => self.lower_operation(operation, source_info)?,
            Expression::Identifier(identifier) => match identifier {
                Identifier::Variable(var_id) | Identifier::GlobalVar(var_id) => {
                    // this code expects to only be used for identifiers found in stack blocks, so
                    // they act like expressions and not addresses i.e:
                    // let var: i32;
                    // @(var);
                    // we dont want this code to be used for this case:
                    // &= var;
                    // assignments deal with their own identifiers, and here we expect to only need to load.
                    // note that this code also loads from global variables with the same semantics as locals.
                    let address = *self
                        .arena
                        .var_id_to_alloca_map
                        .get(&var_id)
                        .expect("[internal error] tried loading from a variable that hasn't been alloca'd yet");
                    let type_id = self.ast_arena.get_var(var_id).hfs_type;
                    self.arena.alloc_inst_for(
                        Instruction::Load { source_info, address, type_id },
                        self.arena.cfg_context.curr_insert_block,
                    )
                },
                Identifier::Function(func_id) => panic!("[internal error] can't have function identifiers as expressions."),
            },
            Expression::Literal(literal) =>
                self.arena.alloc_inst_for(Instruction::Literal(source_info, literal), self.arena.cfg_context.curr_insert_block),
            Expression::Tuple { expressions } => {
                let mut instructions = Vec::<InstId>::new();
                for expr_id in expressions {
                    instructions.push(self.lower_expr(expr_id)?);
                }
                self.arena
                    .alloc_inst_for(Instruction::Tuple { source_info, instructions }, self.arena.cfg_context.curr_insert_block)
            },
            Expression::Parameter { index, type_id } =>
            // NOTE: Expression::Parameter are weird because this instruction isnt really used or
            // will even really be lowered to any assembly in reality. it probably doesnt need to
            // be added to any block, it can be kept inside the CfgFunction struct
                self.arena.alloc_inst_for(
                    Instruction::Parameter { source_info, index, type_id },
                    self.arena.cfg_context.curr_insert_block,
                ),
            Expression::ReturnValue(type_id) => self
                .arena
                .alloc_inst_for(Instruction::ReturnValue { source_info, type_id }, self.arena.cfg_context.curr_insert_block),
            Expression::StackKeyword(StackKeyword { name, parameter_exprs, param_type, return_type, return_values }) => {
                let func_id = self.lower_stack_keyword_declaration(id)?;
                let decl = self.ast_arena.get_stack_keyword_from_name(name.as_str());
                let mut inst_args = Vec::<InstId>::new();
                let arg_count = match decl.expected_args_size {
                    Some(count) => count,
                    None => self.arena.hfs_stack.len(),
                };

                for arg in 0..arg_count {
                    let arg_inst = match self.arena.hfs_stack.pop() {
                        Some(inst) => inst,
                        None =>
                            return cfg_analyzer_error!(
                                CfgAnalyzerErrorKind::StackUnderflow,
                                &self.arena,
                                Some(&self.ast_arena),
                                vec![self.ast_arena.get_expr_token(id).source_info.clone()]
                            ),
                    };
                    inst_args.push(arg_inst);
                }

                let mut new_return_vals = Vec::new();
                for val in return_values {
                    let source_info = self.ast_arena.get_expr_token(val).source_info.clone();
                    let Expression::ReturnValue(type_id) = self.ast_arena.get_expr(val) else {
                        panic!("[internal error] expected Expression::ReturnValue in return_values")
                    };
                    let retval = self.arena.alloc_inst_for(
                        Instruction::ReturnValue { source_info, type_id: *type_id },
                        self.arena.cfg_context.curr_insert_block,
                    );
                    self.arena.push_to_hfs_stack(retval); // keep the stack state correct
                    new_return_vals.push(retval);
                }
                self.arena.alloc_inst_for(
                    Instruction::FunctionCall {
                        source_info,
                        args: inst_args,
                        func_id,
                        is_move: true,
                        return_values: new_return_vals,
                    },
                    self.arena.cfg_context.curr_insert_block,
                )
            },
        };
        self.lowered_expr_cache.insert(id, inst_id);
        Ok(inst_id)
    }
    fn lower_operation(&mut self, op: Operation, source_info: SourceInfo) -> Result<InstId, Box<dyn CompileError>> {
        let cfg_op = match op {
            Operation::Add(expr_id, expr_id1) => CfgOperation::Add(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::Sub(expr_id, expr_id1) => CfgOperation::Sub(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::Mul(expr_id, expr_id1) => CfgOperation::Mul(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::Div(expr_id, expr_id1) => CfgOperation::Div(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::Mod(expr_id, expr_id1) => CfgOperation::Mod(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::Equal(expr_id, expr_id1) => CfgOperation::Equal(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::NotEqual(expr_id, expr_id1) =>
                CfgOperation::NotEqual(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::Less(expr_id, expr_id1) => CfgOperation::Less(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::LessEqual(expr_id, expr1) => CfgOperation::LessEqual(self.lower_expr(expr_id)?, self.lower_expr(expr1)?),
            Operation::Greater(expr_id, expr_id1) => CfgOperation::Greater(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::GreaterEqual(expr, expr1) => CfgOperation::GreaterEqual(self.lower_expr(expr)?, self.lower_expr(expr1)?),
            Operation::Or(expr_id, expr_id1) => CfgOperation::Or(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::And(expr_id, expr_id1) => CfgOperation::And(self.lower_expr(expr_id)?, self.lower_expr(expr_id1)?),
            Operation::Not(expr_id) => CfgOperation::Not(self.lower_expr(expr_id)?),
            Operation::AddressOf(expr_id) => todo!(),
            Operation::Dereference(expr_id) => todo!(),
        };
        Ok(self.arena.alloc_inst_for(Instruction::Operation(source_info, cfg_op), self.arena.cfg_context.curr_insert_block))
    }
}
// impl IrArena {
// ---------------------------------------------------------------------------------
// Simple and Efficient Construction of Static Single Assignment Form (Braun et. al)
// ---------------------------------------------------------------------------------
// TODO: this is code that can be reused later for mem2reg
// fn write_variable(&mut self, var_id: IrVarId, block_id: BlockId, value: InstId) {
//     // keep track of what a variable is while in a given block simply
//     self.current_def.insert((var_id, block_id), value);
// }
// fn read_variable(&mut self, var_id: IrVarId, block_id: BlockId) -> InstId {
//     // Check if we have a def in this block
//     if let Some(&val) = self.current_def.get(&(var_id, block_id)) {
//         return val;
//     }
//     // Otherwise, look in predecessors
//     self.read_variable_recursive(var_id, block_id)
// }
// fn read_variable_recursive(&mut self, var_id: IrVarId, block_id: BlockId) -> InstId {
//     let source_info = self.get_var(var_id).source_info.clone();
//     let val = if !self.sealed_blocks.contains(&block_id) {
//         // Incomplete CFG
//         let phi = self.alloc_inst_for(Instruction::Phi { source_info, incoming: HashMap::new() }, block_id);
//         self.incomplete_phis.entry(block_id).or_default().insert(var_id, phi);
//         phi
//     } else if self.get_block(block_id).predecessors.is_empty() {
//         // this is the entry block which has no predecessors,
//         // so the variable was used before definition
//         panic!("variable '{}' read before initialization", self.get_var(var_id).name)
//     } else if self.get_block(block_id).predecessors.len() == 1 {
//         // Optimize the common case of one predecessor: No phi needed
//         self.read_variable(var_id, self.get_block(block_id).predecessors[0])
//     } else {
//         // Break potential cycles with operandless phi
//         let phi = self.alloc_inst_for(Instruction::Phi { source_info, incoming: HashMap::new() }, block_id);
//         self.write_variable(var_id, block_id, phi);
//         self.add_phi_operands(var_id, phi, block_id)
//     };
//     self.write_variable(var_id, block_id, val);
//     val
// }
// fn add_phi_operands(&mut self, var_id: IrVarId, phi_id: InstId, block_id: BlockId) -> InstId {
//     // block_id is the block that this phi comes from
//     // Determine operands from predecessors
//     let mut new_incoming = Vec::<(BlockId, InstId)>::new();
//     let predecessors = self.get_block(block_id).predecessors.clone();
//     for pred in predecessors {
//         let val = self.read_variable(var_id, pred);
//         new_incoming.push((pred, val));
//     }
//     let Instruction::Phi { incoming, .. } = self.get_instruction_mut(phi_id) else {
//         panic!("[internal error] called add_phi_operands with a non-phi instruction")
//     };
//     for (block_id, inst_id) in new_incoming {
//         incoming.insert(block_id, inst_id);
//     }
//     self.try_remove_trivial_phi(phi_id)
// }
// fn seal_block(&mut self, block_id: BlockId) {
//     // Fill in any incomplete phis we created while block was unsealed
//     if let Some(incomplete) = self.incomplete_phis.remove(&block_id) {
//         for (var, phi) in incomplete {
//             self.add_phi_operands(var, phi, block_id);
//         }
//     }
//     self.sealed_blocks.insert(block_id);
// }
//
// fn try_remove_trivial_phi(&mut self, phi_id: InstId) -> InstId {
//     // TODO:
//     // we wont implement this yet, it requires tracking usages of phis throughout the code.
//     // its part of the algorithm but not necessary for correctness
//     phi_id
// }
// }

// Debug printing functions (using the MIR syntax)
impl CfgAnalyzer {
    pub fn print_hfs_mir(&self, top_level_nodes: Vec<CfgTopLevelId>) {
        for id in top_level_nodes {
            match id {
                CfgTopLevelId::GlobalVarDecl(var_id) => {
                    let var = self.arena.get_var(var_id);
                    println!("{}", var.get_repr(&self.arena));
                },
                CfgTopLevelId::FunctionDecl(func_id) => {
                    let func = self.arena.get_func(func_id);
                    println!("{}", func.get_repr(&self.arena));
                },
            }
        }
    }
}
