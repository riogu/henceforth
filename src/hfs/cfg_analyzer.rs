use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::hfs::{
    self, BasicBlock, BlockId, CfgFunction, CfgOperation, CfgPrintable, CfgTopLevelId, InstId, Instruction, IrFuncId,
    IrVarDeclaration, IrVarId, Literal, PRIMITIVE_TYPE_COUNT, SourceInfo, TermInstId, TerminatorInst, Token, TokenKind, ast::*,
};

// here is where youll create the CFG pass and the new IR generation

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct BlockContext {
    continue_to_block: BlockId,
    break_to_block: BlockId,
    end_block: BlockId,
    // its useful for stuff like break statements
}

#[derive(Debug, Default)]
pub struct IRContext {
    pub curr_block_context: BlockContext,
    pub curr_insert_block: BlockId,
    pub curr_function: IrFuncId,
    block_parent_functions: HashMap<BlockId, IrFuncId>,
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

    pub type_tokens: Vec<Token>,
    type_cache: HashMap<Type, TypeId>,

    pub hfs_stack: Vec<InstId>, // TODO: use this in the code

    pub ir_context: IRContext,
}

impl InstArena {
    pub fn new() -> Self {
        let mut arena = Self::default();
        arena.alloc_type_uncached(Type::Int, Token { kind: TokenKind::Int, source_info: SourceInfo::new(0, 0, 0) });
        arena.alloc_type_uncached(Type::Float, Token { kind: TokenKind::Float, source_info: SourceInfo::new(0, 0, 0) });
        arena.alloc_type_uncached(Type::Bool, Token { kind: TokenKind::Bool, source_info: SourceInfo::new(0, 0, 0) });
        arena.alloc_type_uncached(Type::String, Token { kind: TokenKind::String, source_info: SourceInfo::new(0, 0, 0) });
        arena
    }
    // fn push_to_hfs_stack(&mut self, inst: InstId) {
    //     self.hfs_stack.push(inst);
    // }

    fn alloc_type_uncached(&mut self, hfs_type: Type, token: Token) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type.clone());
        self.type_tokens.push(token);
        id
    }
    pub fn alloc_type(&mut self, hfs_type: Type, token: Token) -> TypeId {
        // Check if this type already exists
        if let Some(&existing_id) = self.type_cache.get(&hfs_type) {
            return existing_id;
        } // If not, allocate it
        self.alloc_type_uncached(hfs_type, token)
    }
    fn alloc_function(&mut self, func: CfgFunction, old_id: FuncId) -> IrFuncId {
        let id = IrFuncId(self.functions.len());
        self.functions.push(func);
        self.func_id_map.insert(old_id, id);
        self.ir_context.curr_function = id;
        id
    }
    fn alloc_inst(&mut self, inst: Instruction) -> InstId {
        let id = InstId(self.instructions.len());
        self.instructions.push(inst);
        id
    }
    pub fn alloc_var(&mut self, var: IrVarDeclaration, old_id: VarId) -> IrVarId {
        let id = IrVarId(self.vars.len());
        self.vars.push(var);
        self.var_id_map.insert(old_id, id);
        id
    }
    pub fn alloc_terminator_for(&mut self, terminator: TerminatorInst, block_id: BlockId) -> TermInstId {
        // add predecessors whenever we jump or branch somewhere
        match terminator {
            TerminatorInst::Branch { ref source_info, cond, true_block, false_block } => {
                self.get_block_mut(true_block).predecessors.push(block_id);
                self.get_block_mut(false_block).predecessors.push(block_id);
            },
            TerminatorInst::Jump(ref source_info, jump_to_id, value) => {
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
            name: name.to_string(),
            predecessors: Vec::new(), // always empty, filled by alloc_terminator_for
            instructions: Vec::new(),
            terminator: None,
        });
        // this block belongs to the current function
        if self.ir_context.block_parent_functions.insert(id, self.ir_context.curr_function).is_some() {
            panic!("[internal error] this block id already had a function associated to it")
        }
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
    pub fn get_terminator_instruction(&self, id: TermInstId) -> &TerminatorInst {
        &self.terminators[id.0]
    }
    pub fn get_block(&self, id: BlockId) -> &BasicBlock {
        &self.blocks[id.0]
    }
    pub fn get_block_mut(&mut self, id: BlockId) -> &mut BasicBlock {
        &mut self.blocks[id.0]
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
            self.arena.hfs_stack.push(param_inst);
        }

        let entry_block = self.arena.alloc_block("start");
        let exit_block = self.arena.alloc_block("end");

        self.arena.ir_context.curr_insert_block = entry_block;
        self.lower_stmt(func_decl.body);

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
    fn lower_stmt(&mut self, id: StmtId) {
        self.arena.analyzed_stmts.insert(id);
        let source_info = self.ast_arena.get_stmt_token(id).source_info.clone();
        match self.ast_arena.get_stmt(id).clone() {
            Statement::Else(stmt_id) => self.lower_stmt(stmt_id),
            if_stmt @ Statement::ElseIf { cond, body, else_stmt } | if_stmt @ Statement::If { cond, body, else_stmt } => {
                // TODO: dont forget about issues with dead code elimination. if we have code after
                // a return; or something, we gotta be careful to eliminate it at some point
                // otherwise we might have issues. maybe we should do that in StackAnalyzer instead?
                // watch out for accidentally overwriting the old terminator if we run this code without dead code elimination
                // basically, this code expects dead code elimination to have occurred BEFORE
                // FIXME: implement a small dead code elimination on the AST before cfg_analyzer
                //
                // FIXME: add checking of stack depth in each branch individually and type check
                // the stack against the current level on other branches and at the end make sure
                // they all tally up to the same depth and types

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
                let block_before_if = self.arena.ir_context.curr_insert_block;
                let cond = self.lower_expr(cond);

                let if_body_block = self.arena.alloc_block("if_body");

                let if_end_block = if matches!(if_stmt, Statement::If { .. }) {
                    self.arena.alloc_block("if_end")
                } else {
                    self.arena.ir_context.curr_block_context.end_block // keep using the same exit if we are in an else if chain
                };

                self.arena.ir_context.curr_block_context.end_block = if_end_block;
                self.arena.ir_context.curr_insert_block = if_body_block;
                self.lower_stmt(body);
                if self.arena.get_block(self.arena.ir_context.curr_insert_block).terminator.is_none() {
                    // if there was no break or return or anything, we need to add a terminator for
                    // whatever block we were on that goes to the end of the current if context
                    self.arena.alloc_terminator_for(
                        TerminatorInst::Jump(source_info.clone(), if_end_block, None),
                        self.arena.ir_context.curr_insert_block,
                    );
                }

                if let Some(else_id) = else_stmt {
                    let else_stmt = self.ast_arena.get_stmt(else_id);
                    let name = if let Statement::ElseIf { .. } = else_stmt {
                        "else_if_cond".to_string()
                    } else {
                        "else_body".to_string()
                    };
                    match else_stmt {
                        Statement::Else(_) | Statement::ElseIf { .. } => {
                            let else_body_block = self.arena.alloc_block(&name);

                            // branch instruction for the original if
                            let if_branch_inst = self.arena.alloc_terminator_for(
                                TerminatorInst::Branch {
                                    source_info: source_info.clone(),
                                    cond,
                                    true_block: if_body_block,
                                    false_block: else_body_block,
                                },
                                block_before_if,
                            );

                            self.arena.ir_context.curr_insert_block = else_body_block;
                            self.lower_stmt(else_id);
                            if self.arena.get_block(self.arena.ir_context.curr_insert_block).terminator.is_none() {
                                // if there was no break or return or anything, we need to add a terminator for
                                // whatever block we were on that goes to the end of the current if context
                                self.arena.alloc_terminator_for(
                                    TerminatorInst::Jump(source_info.clone(), if_end_block, None),
                                    self.arena.ir_context.curr_insert_block,
                                );
                            }
                        },
                        _ => panic!("can't have other statements in else statement"),
                    }
                } else {
                    /* branch 1 < 20, if_body_0, if_end_0;
                       if_body_0:
                           jump if_end_0, (69, 420);
                       if_end_0:
                           return (4, 6);
                    */
                    self.arena.alloc_terminator_for(
                        TerminatorInst::Branch { source_info, cond, true_block: if_body_block, false_block: if_end_block },
                        block_before_if,
                    );
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
                let while_body_block = self.arena.alloc_block("while_body");
                let while_end_block = self.arena.alloc_block("while_end");

                self.arena.alloc_terminator_for(
                    TerminatorInst::Jump(source_info.clone(), while_cond_block, None),
                    self.arena.ir_context.curr_insert_block,
                ); // finish the previous block with a jump

                //--------------------------------------------------------------------------
                // set up the context for lowering the while body
                self.arena.ir_context.curr_insert_block = while_cond_block;
                self.arena.ir_context.curr_block_context.continue_to_block = while_cond_block;
                self.arena.ir_context.curr_block_context.break_to_block = while_end_block;

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

                self.lower_stmt(body);
                if self.arena.get_block(self.arena.ir_context.curr_insert_block).terminator.is_none() {
                    self.arena.alloc_terminator_for(
                        TerminatorInst::Jump(source_info, while_end_block, None),
                        self.arena.ir_context.curr_insert_block,
                    );
                }
                //--------------------------------------------------------------------------
            },
            Statement::StackBlock(expr_ids) =>
                for expr_id in expr_ids {
                    let inst = self.lower_expr(expr_id);
                    self.arena.hfs_stack.push(inst);
                },
            Statement::BlockScope(top_level_ids, scope_kind) =>
                for top_level_id in top_level_ids.clone() {
                    match top_level_id {
                        TopLevelId::VariableDecl(var_id) => {
                            self.lower_variable_declaration(var_id);
                        },
                        TopLevelId::FunctionDecl(func_id) => panic!("[internal error] local functions are not allowed"),
                        TopLevelId::Statement(stmt_id) => self.lower_stmt(stmt_id),
                    };
                },
            Statement::Return => {
                // note that we have already checked that the state of the stack is correct in
                // terms of size and types before each return statement.
                // what the cfg_analyzer does is also check all branches against each other.
                let return_tuple = self.arena.alloc_inst(Instruction::Tuple {
                    source_info: source_info.clone(),
                    instructions: self.arena.hfs_stack.clone(),
                });
                let term = self.arena.alloc_terminator_for(
                    TerminatorInst::Return(source_info, return_tuple),
                    self.arena.ir_context.curr_insert_block,
                );
                self.arena.get_block_mut(self.arena.ir_context.curr_insert_block).terminator = Some(term);
            },
            Statement::Break => {
                // a break always ends the current block and jumps somewhere else
                // (the exit of the current control flow construct)
                self.arena.alloc_terminator_for(
                    TerminatorInst::Jump(source_info, self.arena.ir_context.curr_block_context.break_to_block, None),
                    self.arena.ir_context.curr_insert_block,
                );
            },
            Statement::Continue => {
                // the entry_block is meant to be the block that we came from to start this current
                // context. its meant to allow the start of the next iteration
                self.arena.alloc_terminator_for(
                    TerminatorInst::Jump(source_info, self.arena.ir_context.curr_block_context.continue_to_block, None),
                    self.arena.ir_context.curr_insert_block,
                );
            },
            Statement::Empty => { /* do nothing */ },
            Statement::Assignment { identifier, is_move } => {
                // @(213) &= var; // move assignment
                // @(213) := var; // copy assignment
                // NOTE: we are analyzing "value" twice. this is analyzed earlier by the
                // stack block that contained it.
                // i sure hope we can't have side effects from stuff you push to the stack else
                // this will be a huge source of bugs in the future...
                // we need to add fancier stack simulation to completely get rid of duplication
                // associated to @() stack blocks

                let inst_value = if is_move {
                    self.arena.hfs_stack.pop().expect("expected value in stack for move assignment")
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
                self.arena.alloc_inst(Instruction::Store { source_info, value: inst_value, var_id, is_move });
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
                self.arena.alloc_inst(Instruction::FunctionCall { source_info, args: inst_args, func_id, is_move });
            },
        }
    }
    fn lower_expr(&mut self, id: ExprId) -> InstId {
        let source_info = self.ast_arena.get_expr_token(id).source_info.clone();
        match self.ast_arena.get_expr(id).clone() {
            Expression::Operation(operation) => self.lower_operation(operation, source_info),
            Expression::Identifier(identifier) => match identifier {
                Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => match self.arena.var_id_map.get(&var_id) {
                    Some(ir_var_id) => self.arena.alloc_inst(Instruction::Load(source_info, *ir_var_id)),
                    None => panic!("[internal error] tried making assignment before creating the associated IrVarId"),
                },
                Identifier::Function(func_id) => panic!("[internal error] can't have function identifiers as expressions."),
            },
            Expression::Literal(literal) => self.arena.alloc_inst(Instruction::Literal(source_info, literal)),
            Expression::Tuple { expressions } => {
                let mut instructions = Vec::<InstId>::new();
                for expr_id in expressions {
                    instructions.push(self.lower_expr(expr_id));
                }
                self.arena.alloc_inst(Instruction::Tuple { source_info, instructions })
            },
            Expression::Parameter { index, type_id } =>
            // not sure if this is correct for when you hold a parameter expr throughout a function
            // its probably fine but just in case maybe test smth like that later
                self.arena.alloc_inst(Instruction::Parameter { source_info, index, type_id }),
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
        self.arena.alloc_inst(Instruction::Operation(source_info, cfg_op))
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
