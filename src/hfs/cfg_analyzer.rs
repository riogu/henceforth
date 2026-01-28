use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::hfs::{
    self, BasicBlock, BlockId, CfgFunction, CfgOperation, CfgPrintable, CfgTopLevelId, InstId, Instruction, IrFuncId,
    IrVarDeclaration, IrVarId, Literal, PRIMITIVE_TYPE_COUNT, SourceInfo, TermInstId, TerminatorInst, Token, TokenKind, ast::*,
};

// here is where youll create the CFG pass and the new IR generation

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BlockContext {
    continue_to_block: Option<BlockId>,
    break_to_block: Option<BlockId>,
    end_block: Option<BlockId>,
    prev_stack_change: Vec<InstId>,
    stack_changes: Vec<(BlockId, Vec<InstId>)>,
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
pub struct InstArena {
    // TODO: we also need to pass metadata into this new IR
    // which includes expression provenance and etc (for optimizations and annotations)
    // we should make an annotation system and convert into it
    pub vars: Vec<IrVarDeclaration>,
    pub functions: Vec<CfgFunction>,
    pub instructions: Vec<Instruction>,
    pub terminators: Vec<TerminatorInst>,
    pub blocks: Vec<BasicBlock>,
    pub types: Vec<Type>,

    func_id_map: HashMap<FuncId, IrFuncId>,
    var_id_map: HashMap<VarId, IrVarId>,

    analyzed_stmts: HashSet<StmtId>,
    analyzed_exprs: HashSet<ExprId>,

    pub type_source_infos: Vec<SourceInfo>,
    type_cache: HashMap<Type, TypeId>,

    hfs_stack: Vec<InstId>,
    // this is reset whenever we add a terminator instruction to the current block
    // its used to keep track of each merging stack
    pub curr_block_stack: Vec<InstId>,

    pub cfg_context: CfgContext,

    // For each variable, in each block, what's the current definition?
    current_def: HashMap<(IrVarId, BlockId), InstId>,
    // Which blocks have all their predecessors known?
    sealed_blocks: HashSet<BlockId>,
    // Placeholder phis waiting for block to be sealed
    incomplete_phis: HashMap<BlockId, HashMap<IrVarId, InstId>>,
    /*
        TODO:
        The stack is just Vec<InstId> - a list of values. At a join point, you merge each slot independently:
        if_body ends with stack: [%1, %2, %3]
        else_body ends with stack: [%4, %5, %6]

        if_end gets:
            stack[0] = phi [(if_body, %1), (else_body, %4)]
            stack[1] = phi [(if_body, %2), (else_body, %5)]
            stack[2] = phi [(if_body, %3), (else_body, %6)]
        No tuples involved in the IR. Just track HashMap<BlockId, Vec<InstId>> for the stack state when leaving each block.
        The tuples only matter later when you emit LLVM IR and need to actually pass multiple return values or whatever.
        That's a codegen concern, not an SSA construction concern.
    */
}

impl InstArena {
    pub fn new() -> Self {
        let mut arena = Self::default();
        arena.alloc_type_uncached(Type::Int, SourceInfo::new(0, 0, 0));
        arena.alloc_type_uncached(Type::Float, SourceInfo::new(0, 0, 0));
        arena.alloc_type_uncached(Type::Bool, SourceInfo::new(0, 0, 0));
        arena.alloc_type_uncached(Type::String, SourceInfo::new(0, 0, 0));
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
    fn alloc_inst_for(&mut self, inst: Instruction, block_id: BlockId) -> InstId {
        let id = InstId(self.instructions.len());
        self.instructions.push(inst);
        self.get_block_mut(block_id).instructions.push(id);
        id
    }
    pub fn alloc_var(&mut self, var: IrVarDeclaration, old_id: VarId) -> IrVarId {
        let id = IrVarId(self.vars.len());
        self.vars.push(var);
        self.var_id_map.insert(old_id, id);
        id
    }
    pub fn alloc_terminator_for(&mut self, mut terminator: TerminatorInst, block_id: BlockId) -> TermInstId {
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
        id
    }
    pub fn alloc_block(&mut self, name: &str) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(BasicBlock {
            parent_function: self.cfg_context.curr_function,
            name: name.to_string(),
            predecessors: Vec::new(), // always empty, filled by alloc_terminator_for
            instructions: Vec::new(),
            terminator: None,
        });
        id
    }
}

impl InstArena {
    pub fn pop_entire_hfs_stack(&mut self) -> Vec<InstId> {
        let temp = self.hfs_stack.clone();
        self.hfs_stack.clear();
        temp
    }

    pub fn get_var(&self, id: IrVarId) -> &IrVarDeclaration {
        &self.vars[id.0]
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

    pub fn compare_stacks(&mut self, stack1: &Vec<InstId>, stack2: &Vec<InstId>) -> Result<(), String> {
        let expected_count = stack1.len();
        let actual_count = stack2.len();
        if expected_count != actual_count {
            return Err(format!("expected {} values on stack for return, found {}", expected_count, actual_count));
        }

        for (inst_id1, inst_id2) in stack1.iter().zip(stack2.iter()) {
            let type_id1 = self.get_type_id_of_inst(*inst_id1);
            let type_id2 = self.get_type_id_of_inst(*inst_id2);
            self.compare_types(type_id1, type_id2)?;
        }
        Ok(())
    }

    pub fn compare_types(&self, type1: TypeId, type2: TypeId) -> Result<(), String> {
        let actual_type = self.get_type(type1);
        let expected_type = self.get_type(type2);

        match (actual_type, expected_type) {
            (Type::Tuple(actual_types), Type::Tuple(expected_types)) => {
                if actual_types.len() != expected_types.len() {
                    return Err(format!(
                        "tuple length mismatch: expected {} elements, found {}",
                        expected_types.len(),
                        actual_types.len()
                    ));
                }
                // Recursively validate each element
                for (i, (&actual_elem_id, &expected_elem_id)) in actual_types.iter().zip(expected_types.iter()).enumerate() {
                    self.compare_types(actual_elem_id, expected_elem_id)
                        .map_err(|err| format!("in tuple element {}: {}", i, err))?;
                }
                Ok(())
            },
            (actual, expected) if actual == expected => Ok(()),
            (actual, expected) => Err(format!("type mismatch: expected '{:?}', found '{:?}'", expected, actual)),
        }
    }
}

#[derive(Debug)]
pub struct CfgAnalyzer {
    pub ast_arena: AstArena,
    pub arena: InstArena,
    // similar to StackAnalyzer, pretty much that
}

impl CfgAnalyzer {
    pub fn new(ast_arena: AstArena) -> Self {
        let mut arena = InstArena::new();
        arena.types.extend_from_slice(&ast_arena.types[PRIMITIVE_TYPE_COUNT..]);
        Self { ast_arena, arena }
    }

    pub fn lower_to_mir(top_level: Vec<TopLevelId>, ast_arena: AstArena) -> InstArena {
        let mut cfg_analyzer = CfgAnalyzer::new(ast_arena);
        let analyzed_top_level = cfg_analyzer.lower_top_level(top_level);
        // cfg_analyzer.print_hfs_mir(analyzed_top_level);
        cfg_analyzer.arena
    }

    fn lower_top_level(&mut self, top_level: Vec<TopLevelId>) -> Vec<CfgTopLevelId> {
        let mut analyzed_nodes = Vec::<CfgTopLevelId>::new();
        for node in top_level.clone() {
            let new_node = match node {
                TopLevelId::VariableDecl(id) => CfgTopLevelId::GlobalVarDecl(self.lower_variable_declaration(id)),
                TopLevelId::FunctionDecl(id) => CfgTopLevelId::FunctionDecl(self.lower_function_declaration(id)),
                TopLevelId::Statement(id) => panic!("there can't be statements on global scope"),
            };
            analyzed_nodes.push(new_node);
        }
        analyzed_nodes
    }

    fn lower_variable_declaration(&mut self, id: VarId) -> IrVarId {
        // we don't do anything here at all right now
        // maybe if we add assignments to declarations we might want to in the future but for now this doesn't do anything
        let var = self.ast_arena.get_var(id);
        self.arena.alloc_var(
            IrVarDeclaration {
                source_info: self.ast_arena.get_var_token(id).source_info.clone(),
                name: var.name.clone(),
                hfs_type: var.hfs_type,
                is_global: true,
            },
            id,
        )
    }

    fn lower_function_declaration(&mut self, id: FuncId) -> IrFuncId {
        let func_decl = self.ast_arena.get_func(id).clone();
        // self.arena.alloc_function(func, self.ast_arena.get_function_token(id));
        let mut parameter_exprs = Vec::<InstId>::new();
        for param_expr_id in func_decl.parameter_exprs {
            let param_inst = self.lower_expr(param_expr_id);
            parameter_exprs.push(param_inst);
            self.arena.push_to_hfs_stack(param_inst);
        }

        let entry_block = self.arena.alloc_block("start");
        self.arena.seal_block(entry_block);
        self.arena.cfg_context.curr_insert_block = entry_block;

        let curr_block_context = BlockContext {
            continue_to_block: None,
            break_to_block: None,
            end_block: None,
            prev_stack_change: vec![],
            stack_changes: vec![],
        };
        self.lower_stmt(func_decl.body, curr_block_context);

        let cfg_function = CfgFunction {
            source_info: self.ast_arena.get_function_token(id).source_info.clone(),
            name: func_decl.name,
            param_type: func_decl.param_type,
            return_type: func_decl.return_type,
            parameter_exprs,
            entry_block,
        };

        self.arena.pop_entire_hfs_stack();
        self.arena.alloc_function(cfg_function, id)
    }
    fn lower_stmt(&mut self, id: StmtId, curr_block_context: BlockContext) {
        self.arena.analyzed_stmts.insert(id);
        let source_info = self.ast_arena.get_stmt_token(id).source_info.clone();
        match self.ast_arena.get_stmt(id).clone() {
            Statement::Else(stmt_id) => self.lower_stmt(stmt_id, curr_block_context),
            if_stmt @ Statement::ElseIf { cond_stack_block, body, else_stmt }
            | if_stmt @ Statement::If { cond_stack_block, body, else_stmt } => {
                // TODO: dont forget about issues with dead code elimination. if we have code after
                // a return; or something, we gotta be careful to eliminate it at some point
                // otherwise we might have issues. maybe we should do that in StackAnalyzer instead?
                // watch out for accidentally overwriting the old terminator if we run this code without dead code elimination
                // basically, this code expects dead code elimination to have occurred BEFORE
                // FIXME: implement a small dead code elimination on the AST before cfg_analyzer

                // TODO: add the missing stack merging code!
                // without this, the code wont work

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
                let mut stack_changes = curr_block_context.stack_changes.clone();

                // means we are starting a new chain of ifs
                let new_if_context = matches!(if_stmt, Statement::If { .. });
                let stack_before_branches = self.arena.hfs_stack.clone();

                let block_before_if = self.arena.cfg_context.curr_insert_block;
                self.lower_stmt(cond_stack_block, curr_block_context.clone());

                let if_body_block = self.arena.alloc_block("if_body");
                self.arena.cfg_context.curr_insert_block = if_body_block;

                // condition isnt included in the stack depth count
                let cond = self.arena.pop_hfs_stack().expect("expected condition at the end of stack block condition in if_stmt");
                let if_depth_before = self.arena.hfs_stack.len();

                self.lower_stmt(body, curr_block_context.clone()); // pay attention to this call (we manage state around it)

                let if_stack_change = self.arena.hfs_stack[if_depth_before..].to_vec();
                // FIXME: this wont work if the stack becomes smaller ^^^^
                stack_changes.push((self.arena.cfg_context.curr_insert_block, if_stack_change.clone()));
                // FIXME: verify that calls to self.arena.seal_block() are correctly placed

                let if_end_block = if new_if_context {
                    self.arena.alloc_block("if_end")
                } else {
                    // validate that we aren't getting a different stack depth
                    if let Err(err) = self.arena.compare_stacks(&if_stack_change, &curr_block_context.prev_stack_change) {
                        panic!("found different stacks while evaluating if chain context:\n\t {}", err)
                    }
                    curr_block_context.end_block.expect("[internal error] forgot to set exit block for the current if stmt")
                };

                if self.arena.get_block(self.arena.cfg_context.curr_insert_block).terminator.is_none() {
                    self.arena.alloc_terminator_for(
                        TerminatorInst::Jump(source_info.clone(), if_end_block),
                        self.arena.cfg_context.curr_insert_block,
                    );
                }
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

                            self.arena.seal_block(if_body_block);
                            self.arena.seal_block(else_body_block);

                            self.arena.cfg_context.curr_insert_block = else_body_block;

                            let curr_block_context = BlockContext {
                                continue_to_block: curr_block_context.continue_to_block,
                                break_to_block: curr_block_context.break_to_block,
                                end_block: Some(if_end_block),
                                prev_stack_change: if_stack_change.clone(),
                                stack_changes: stack_changes.clone(),
                            };

                            self.arena.hfs_stack = stack_before_branches.clone(); // restore stack before else
                            self.lower_stmt(else_id, curr_block_context); // pay attention to this call (we manage state around it)
                            let else_stack_change = self.arena.hfs_stack[stack_before_branches.len()..].to_vec();
                            stack_changes.push((self.arena.cfg_context.curr_insert_block, else_stack_change));

                            if self.arena.get_block(self.arena.cfg_context.curr_insert_block).terminator.is_none() {
                                self.arena.alloc_terminator_for(
                                    TerminatorInst::Jump(source_info.clone(), if_end_block),
                                    self.arena.cfg_context.curr_insert_block,
                                );
                            }
                            if is_else {
                                // here we wanna make our phis because we are done collecting all the stacks
                                // there always needs to exist an else at the end, otherwise the chain isn't
                                // allowed to have a stack effect. this means we are done with the context
                                self.arena.hfs_stack = stack_before_branches.clone();
                                for stack_idx in 0..if_stack_change.len() {
                                    // take all stacks and group each element with each other into a phi
                                    // for ex: we group every 1st pushed value together for each branch
                                    let mut incoming = Vec::<(BlockId, InstId)>::new();
                                    for (block_id, stack_change) in &stack_changes {
                                        incoming.push((*block_id, stack_change[stack_idx]));
                                    }
                                    let phi = self.arena.alloc_inst_for(
                                        Instruction::Phi { source_info: source_info.clone(), incoming },
                                        if_end_block,
                                    );
                                    self.arena.hfs_stack.push(phi);
                                }
                                self.arena.cfg_context.curr_insert_block = if_end_block;
                            }
                        },
                        _ => panic!("can't have other statements in else statement"),
                    }
                } else {
                    if self.arena.hfs_stack.len() != if_depth_before {
                        panic!("if statement cannot change the stack depth without an associated 'else' statement.")
                    }
                    self.arena.alloc_terminator_for(
                        TerminatorInst::Branch { source_info, cond, true_block: if_body_block, false_block: if_end_block },
                        block_before_if,
                    );
                    self.arena.seal_block(if_body_block);
                }
                self.arena.seal_block(if_end_block);
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
                let cond = self.lower_expr(cond);
                self.arena.alloc_terminator_for(
                    TerminatorInst::Branch {
                        source_info: source_info.clone(),
                        cond,
                        true_block: while_body_block,
                        false_block: while_end_block,
                    },
                    while_cond_block,
                );
                self.arena.seal_block(while_body_block);

                let curr_block_context = BlockContext {
                    continue_to_block: Some(while_cond_block),
                    break_to_block: Some(while_end_block),
                    end_block: None,
                    prev_stack_change: vec![],
                    stack_changes: vec![],
                };

                let stack_depth_before = self.arena.hfs_stack.len();

                self.lower_stmt(body, curr_block_context);

                // Enforce stack balance
                let stack_depth_after = self.arena.hfs_stack.len();
                if stack_depth_before != stack_depth_after {
                    panic!(
                        "while loop body must maintain stack balance: expected {} values on stack after loop body, found {}",
                        stack_depth_before, stack_depth_after
                    );
                }
                if self.arena.get_block(self.arena.cfg_context.curr_insert_block).terminator.is_none() {
                    self.arena.alloc_terminator_for(
                        TerminatorInst::Jump(source_info, while_cond_block),
                        self.arena.cfg_context.curr_insert_block,
                    );
                }
                self.arena.seal_block(while_end_block);
                self.arena.seal_block(while_cond_block);
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
                for expr_id in expr_ids {
                    let inst = self.lower_expr(expr_id);
                    self.arena.push_to_hfs_stack(inst);
                    // we add stack operations as standalone instructions in the current block (just like normal SSA)
                    // the stack semantics are validated but they act "like local variables"
                    self.arena.get_block_mut(self.arena.cfg_context.curr_insert_block).instructions.push(inst);
                },
            Statement::BlockScope(top_level_ids, scope_kind) =>
                for top_level_id in top_level_ids.clone() {
                    match top_level_id {
                        TopLevelId::VariableDecl(var_id) => {
                            self.lower_variable_declaration(var_id);
                        },
                        TopLevelId::FunctionDecl(func_id) => panic!("[internal error] local functions are not allowed"),
                        TopLevelId::Statement(stmt_id) => self.lower_stmt(stmt_id, curr_block_context.clone()),
                    };
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
                self.arena.get_block_mut(self.arena.cfg_context.curr_insert_block).terminator = Some(term);
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
            },
            Statement::Empty => { /* do nothing */ },
            Statement::Assignment { identifier, is_move } => {
                // @(213) &= var; // move assignment
                // @(213) := var; // copy assignment
                // i sure hope we can't have side effects from stuff you push to the stack else
                // this will be a huge source of bugs in the future...
                // we need to add fancier stack simulation to completely get rid of duplication
                // associated to @() stack blocks

                let inst_value = if is_move {
                    self.arena.pop_hfs_stack().expect("expected value in stack for move assignment")
                } else {
                    *self.arena.hfs_stack.last().expect("expected value in stack for copy assignment")
                };
                let &var_id = match identifier {
                    Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => match self.arena.var_id_map.get(&var_id) {
                        Some(ir_var_id) => ir_var_id,
                        None => panic!("[internal error] tried making an assignment before creating the associated IrVarId"),
                    },
                    Identifier::Function(func_id) => unreachable!("can't happen"),
                };
                self.arena.write_variable(var_id, self.arena.cfg_context.curr_insert_block, inst_value);
            },
            Statement::FunctionCall { arg_count, func_id, is_move } => {
                // @(213) &> func; // move call
                // @(213) :> func; // copy call
                let mut inst_args = Vec::<InstId>::new();
                for arg in 0..arg_count {
                    inst_args.push(self.arena.hfs_stack.pop().expect("expected value in stack for function call"));
                }
                let func_id = match self.arena.func_id_map.get(&func_id) {
                    Some(func_id) => *func_id,
                    None => panic!("[internal error] tried making a function call before creating the associated IrFuncId"),
                };
                self.arena.alloc_inst_for(
                    Instruction::FunctionCall { source_info, args: inst_args, func_id, is_move },
                    self.arena.cfg_context.curr_insert_block,
                );
            },
        }
    }
    fn lower_expr(&mut self, id: ExprId) -> InstId {
        let source_info = self.ast_arena.get_expr_token(id).source_info.clone();
        match self.ast_arena.get_expr(id).clone() {
            Expression::Operation(operation) => self.lower_operation(operation, source_info),
            Expression::Identifier(identifier) => match identifier {
                Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => match self.arena.var_id_map.get(&var_id) {
                    Some(ir_var_id) => {
                        // phi code generation happens when you read a variable
                        self.arena.read_variable(*ir_var_id, self.arena.cfg_context.curr_insert_block)
                    },
                    None => panic!("[internal error] tried making assignment before creating the associated IrVarId"),
                },
                Identifier::Function(func_id) => panic!("[internal error] can't have function identifiers as expressions."),
            },
            Expression::Literal(literal) =>
                self.arena.alloc_inst_for(Instruction::Literal(source_info, literal), self.arena.cfg_context.curr_insert_block),
            Expression::Tuple { expressions } => {
                let mut instructions = Vec::<InstId>::new();
                for expr_id in expressions {
                    instructions.push(self.lower_expr(expr_id));
                }
                self.arena
                    .alloc_inst_for(Instruction::Tuple { source_info, instructions }, self.arena.cfg_context.curr_insert_block)
            },
            Expression::Parameter { index, type_id } =>
            // not sure if this is correct for when you hold a parameter expr throughout a function
            // its probably fine but just in case maybe test smth like that later
                self.arena.alloc_inst_for(
                    Instruction::Parameter { source_info, index, type_id },
                    self.arena.cfg_context.curr_insert_block,
                ),
            Expression::StackKeyword(stack_keyword) => todo!(),
        }
    }
    fn lower_operation(&mut self, op: Operation, source_info: SourceInfo) -> InstId {
        let cfg_op = match op {
            Operation::Add(expr_id, expr_id1) => CfgOperation::Add(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::Sub(expr_id, expr_id1) => CfgOperation::Sub(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::Mul(expr_id, expr_id1) => CfgOperation::Mul(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::Div(expr_id, expr_id1) => CfgOperation::Div(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::Mod(expr_id, expr_id1) => CfgOperation::Mod(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::Equal(expr_id, expr_id1) => CfgOperation::Equal(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::NotEqual(expr_id, expr_id1) => CfgOperation::NotEqual(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::Less(expr_id, expr_id1) => CfgOperation::Less(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::LessEqual(expr_id, expr1) => CfgOperation::LessEqual(self.lower_expr(expr_id), self.lower_expr(expr1)),
            Operation::Greater(expr_id, expr_id1) => CfgOperation::Greater(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::GreaterEqual(expr, expr1) => CfgOperation::GreaterEqual(self.lower_expr(expr), self.lower_expr(expr1)),
            Operation::Or(expr_id, expr_id1) => CfgOperation::Or(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::And(expr_id, expr_id1) => CfgOperation::And(self.lower_expr(expr_id), self.lower_expr(expr_id1)),
            Operation::Not(expr_id) => CfgOperation::Not(self.lower_expr(expr_id)),
        };
        self.arena.alloc_inst_for(Instruction::Operation(source_info, cfg_op), self.arena.cfg_context.curr_insert_block)
    }
}
impl InstArena {
    // ---------------------------------------------------------------------------------
    // Simple and Efficient Construction of Static Single Assignment Form (Braun et. al)
    // ---------------------------------------------------------------------------------
    fn write_variable(&mut self, var_id: IrVarId, block_id: BlockId, value: InstId) {
        // keep track of what a variable is while in a given block simply
        self.current_def.insert((var_id, block_id), value);
    }
    fn read_variable(&mut self, var_id: IrVarId, block_id: BlockId) -> InstId {
        // Check if we have a def in this block
        if let Some(&val) = self.current_def.get(&(var_id, block_id)) {
            return val;
        }
        // Otherwise, look in predecessors
        self.read_variable_recursive(var_id, block_id)
    }
    fn read_variable_recursive(&mut self, var_id: IrVarId, block_id: BlockId) -> InstId {
        let source_info = self.get_var(var_id).source_info.clone();
        let val = if !self.sealed_blocks.contains(&block_id) {
            // Incomplete CFG
            let phi = self.alloc_inst_for(Instruction::Phi { source_info, incoming: vec![] }, block_id);
            self.incomplete_phis.entry(block_id).or_default().insert(var_id, phi);
            phi
        } else if self.get_block(block_id).predecessors.is_empty() {
            // this is the entry block which has no predecessors,
            // so the variable was used before definition
            panic!("variable '{}' read before initialization", self.get_var(var_id).name)
        } else if self.get_block(block_id).predecessors.len() == 1 {
            // Optimize the common case of one predecessor: No phi needed
            self.read_variable(var_id, self.get_block(block_id).predecessors[0])
        } else {
            // Break potential cycles with operandless phi
            let phi = self.alloc_inst_for(Instruction::Phi { source_info, incoming: vec![] }, block_id);
            self.write_variable(var_id, block_id, phi);
            self.add_phi_operands(var_id, phi, block_id)
        };
        self.write_variable(var_id, block_id, val);
        val
    }
    fn add_phi_operands(&mut self, var_id: IrVarId, phi_id: InstId, block_id: BlockId) -> InstId {
        // block_id is the block that this phi comes from
        // Determine operands from predecessors
        let mut new_incoming = Vec::<(BlockId, InstId)>::new();
        let predecessors = self.get_block(block_id).predecessors.clone();
        for pred in predecessors {
            let val = self.read_variable(var_id, pred);
            new_incoming.push((pred, val));
        }
        let Instruction::Phi { incoming, .. } = self.get_instruction_mut(phi_id) else {
            panic!("[internal error] called add_phi_operands with a non-phi instruction")
        };
        incoming.append(&mut new_incoming);
        self.try_remove_trivial_phi(phi_id)
    }
    fn seal_block(&mut self, block_id: BlockId) {
        // Fill in any incomplete phis we created while block was unsealed
        if let Some(incomplete) = self.incomplete_phis.remove(&block_id) {
            for (var, phi) in incomplete {
                self.add_phi_operands(var, phi, block_id);
            }
        }
        self.sealed_blocks.insert(block_id);
    }

    fn try_remove_trivial_phi(&mut self, phi_id: InstId) -> InstId {
        // TODO:
        // we wont implement this yet, it requires tracking usages of phis throughout the code.
        // its part of the algorithm but not necessary for correctness
        phi_id
    }
}

// Debug printing functions (using the MIR syntax)
// FIXME: joao i broke your prints again
// impl CfgAnalyzer {
//     pub fn print_hfs_mir(&self, top_level_nodes: Vec<CfgTopLevelId>) {
//         for id in top_level_nodes {
//             match id {
//                 CfgTopLevelId::GlobalVarDecl(var_id) => {
//                     let var = self.arena.get_var(var_id);
//                     println!("{}", var.get_repr(&self.arena));
//                 },
//                 CfgTopLevelId::FunctionDecl(func_id) => {
//                     let func = self.arena.get_func(func_id);
//                     println!("{}", func.get_repr(&self.arena));
//                 },
//             }
//         }
//     }
// }
