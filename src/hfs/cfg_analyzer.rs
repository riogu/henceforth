use std::{collections::HashMap, fmt::Debug};

use crate::hfs::{
    self, BasicBlock, BlockId, CfgFunction, CfgPrintable, CfgTopLevelId, InstId, Instruction, Literal, PRIMITIVE_TYPE_COUNT,
    SourceInfo, TermInstId, TerminatorInst, Token, TokenKind, ast::*,
};

// here is where youll create the CFG pass and the new IR generation

#[derive(Debug, Default)]
pub struct InstArena {
    // TODO: we also need to pass metadata into this new IR
    // which includes expression provenance and etc (for optimizations and annotations)
    // we should make an annotation system and convert into it
    pub vars: Vec<VarDeclaration>,
    pub functions: Vec<CfgFunction>,
    pub instructions: Vec<Instruction>,
    pub terminators: Vec<TerminatorInst>,
    pub blocks: Vec<BasicBlock>,
    pub types: Vec<Type>,

    pub type_tokens: Vec<Token>,
    type_cache: HashMap<Type, TypeId>,

    pub hfs_stack: Vec<InstId>, // TODO: use this in the code
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
    fn alloc_function(&mut self, func: CfgFunction) -> FuncId {
        let id = FuncId(self.functions.len());
        self.functions.push(func);
        id
    }
    fn alloc_inst(&mut self, inst: Instruction) -> InstId {
        let id = InstId(self.instructions.len());
        self.instructions.push(inst);
        id
    }
    pub fn alloc_var(&mut self, var: VarDeclaration) -> VarId {
        let id = VarId(self.vars.len());
        self.vars.push(var);
        id
    }
    pub fn alloc_terminator(&mut self, terminator: TerminatorInst) -> TermInstId {
        let id = TermInstId(self.terminators.len());
        self.terminators.push(terminator);
        id
    }
    pub fn alloc_block(&mut self, block: BasicBlock) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(block);
        id
    }
}

impl InstArena {
    pub fn get_var(&self, id: VarId) -> &VarDeclaration {
        &self.vars[id.0]
    }
    pub fn get_func(&self, id: FuncId) -> &CfgFunction {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockContext {
    entry_block: BlockId,
    curr_block: BlockId,
    exit_block: BlockId,
    // its useful for stuff like break statements
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

    pub fn analyze(top_level: Vec<TopLevelId>, ast_arena: AstArena) -> InstArena {
        let mut cfg_analyzer = CfgAnalyzer::new(ast_arena);
        let analyzed_top_level = cfg_analyzer.analyze_top_level(top_level);
        cfg_analyzer.print_hfs_mir(analyzed_top_level);
        cfg_analyzer.arena
    }

    fn analyze_top_level(&mut self, top_level: Vec<TopLevelId>) -> Vec<CfgTopLevelId> {
        let mut analyzed_nodes = Vec::<CfgTopLevelId>::new();
        for node in top_level.clone() {
            let new_node = match node {
                TopLevelId::VariableDecl(id) => CfgTopLevelId::GlobalVarDecl(
                    self.ast_arena.get_var_token(id).source_info.clone(),
                    id, // currently we do nothing in particularly in this pass to globals
                ),
                TopLevelId::FunctionDecl(id) => CfgTopLevelId::FunctionDecl(
                    self.ast_arena.get_function_token(id).source_info.clone(),
                    self.analyze_function_declaration(id),
                ),
                TopLevelId::Statement(id) => panic!("there can't be statements on global scope"),
            };
            analyzed_nodes.push(new_node);
        }
        analyzed_nodes
    }

    fn analyze_variable_declaration(&mut self, id: VarId, curr_block_context: BlockContext) -> InstId {
        // we don't do anything here at all right now
        // maybe if we add assignments to declarations we might want to in the future but for now this doesn't do anything
        self.arena.alloc_inst(Instruction::VarDeclaration(self.ast_arena.get_var_token(id).source_info.clone(), id))
    }

    fn analyze_function_declaration(&mut self, id: FuncId) -> FuncId {
        let func_decl = self.ast_arena.get_func(id).clone();
        // self.arena.alloc_function(func, self.ast_arena.get_function_token(id));
        let mut parameter_exprs = Vec::<InstId>::new();
        for param_expr_id in func_decl.parameter_exprs {
            if let &Expression::Parameter { index, type_id } = self.ast_arena.get_expr(param_expr_id) {
                parameter_exprs.push(self.arena.alloc_inst(Instruction::Parameter {
                    source_info: self.ast_arena.get_expr_token(param_expr_id).source_info.clone(),
                    index,
                    type_id,
                }));
            } else {
                panic!("[internal error] found non Expression::Parameter in function parameters")
            }
        }

        let entry_block = self.arena.alloc_block(BasicBlock {
            name: "start".to_string(),
            predecessors: Vec::new(),
            instructions: Vec::new(),
            terminator: TermInstId(0), // placeholder
        });
        let exit_block = self.arena.alloc_block(BasicBlock {
            name: "end".to_string(),
            predecessors: Vec::new(),
            instructions: Vec::new(),
            terminator: TermInstId(0), // placeholder
        });
        self.analyze_stmt(func_decl.body, BlockContext { entry_block, curr_block: entry_block, exit_block });

        let cfg_function = CfgFunction {
            old_func_id: id,
            source_info: self.ast_arena.get_function_token(id).source_info.clone(),
            name: func_decl.name,
            param_type: func_decl.param_type,
            return_type: func_decl.return_type,
            parameter_exprs,
            entry_block,
        };
        self.arena.alloc_function(cfg_function)
    }
    fn analyze_stmt(&mut self, id: StmtId, curr_block_context: BlockContext) {
        let source_info = self.ast_arena.get_stmt_token(id).source_info.clone();
        match self.ast_arena.get_stmt(id).clone() {
            Statement::If { cond, body, else_stmt } => {
                // TODO: dont forget about issues with dead code elimination. if we have code after
                // a return; or something, we gotta be careful to eliminate it at some point
                // otherwise we might have issues. maybe we should do that in StackAnalyzer instead?
                // watch out for accidentally overwriting the old terminator if we run this code without dead code elimination

                /* example of CFG blocks that cover many of the cases of the code below:
                  start_function:
                      jump if_cond_0;
                      if_cond_0:
                          branch var < 2.0, if_block_0, else_if_cond_0;
                          if_block_0:
                              jump end_if_0, (%inst0 %inst1);
                      else_if_cond_0:
                          branch -420 < 5, else_if_block_0, else_if_cond_1;
                          else_if_block_0:
                              %inst2 = push (0 0 0);
                              jump end_if_0, %inst2;
                      else_if_cond_1:
                          branch -69 < 69, else_if_block_0, else_block_0;
                          else_if_block_1:
                              jump end_if_0, %inst6;
                      else_block_0:
                          jump end_if_0, %inst4;
                      end_if_0:
                          jump end_function;
                  end_function:
                    return (1, 2, 3);
                */
                let if_cond_block = self.arena.alloc_block(BasicBlock {
                    name: "if_cond".to_string(), // add numbering later maybe
                    predecessors: vec![curr_block_context.curr_block],
                    instructions: Vec::new(),
                    terminator: TermInstId(0), // placeholder
                });
                let if_body_block = self.arena.alloc_block(BasicBlock {
                    name: "if_body".to_string(), // add numbering later maybe
                    predecessors: vec![if_cond_block],
                    instructions: Vec::new(),
                    terminator: TermInstId(0), // placeholder
                });
                let if_end_block = self.arena.alloc_block(BasicBlock {
                    name: "if_end".to_string(),
                    predecessors: vec![if_body_block],
                    instructions: Vec::new(),
                    terminator: TermInstId(0), // placeholder
                });
                // finish the previous block we were on (as we see in the example above)
                self.arena.alloc_terminator(TerminatorInst::Jump(source_info, if_cond_block, None));
                self.analyze_stmt(body, BlockContext {
                    entry_block: curr_block_context.entry_block,
                    curr_block: if_cond_block,
                    exit_block: if_end_block,
                });

                let source_info = self.ast_arena.get_expr_token(cond).source_info.clone();
                let cond = self.analyze_expr(cond, curr_block_context);

                match else_stmt {
                    Some(ElseStmt::ElseIf(stmt_id)) => {
                        let stmt_id = stmt_id.clone();
                        let else_body_block = self.arena.alloc_block(BasicBlock {
                            name: "else_body".to_string(),
                            predecessors: vec![curr_block_context.curr_block],
                            instructions: Vec::new(),
                            terminator: TermInstId(0), // placeholder
                        });
                        // branch instruction for the original if
                        let if_terminator = self.arena.alloc_terminator(TerminatorInst::Branch {
                            source_info,
                            cond,
                            true_block: if_body_block,
                            false_block: else_body_block,
                        });
                        self.arena.get_block_mut(if_cond_block).terminator = if_terminator;
                        // we have an if statement in the stmt_id
                        self.analyze_stmt(stmt_id, curr_block_context);
                    },
                    Some(ElseStmt::Else(stmt_id)) => {
                        let stmt_id = stmt_id.clone();
                        let else_if_cond_block = self.arena.alloc_block(BasicBlock {
                            name: "else_if_cond".to_string(),
                            predecessors: vec![curr_block_context.curr_block],
                            instructions: Vec::new(),
                            terminator: TermInstId(0), // placeholder
                        });
                        // branch instruction for the original if
                        let if_terminator = self.arena.alloc_terminator(TerminatorInst::Branch {
                            source_info,
                            cond,
                            true_block: if_body_block,
                            false_block: else_if_cond_block,
                        });
                        self.arena.get_block_mut(if_cond_block).terminator = if_terminator;
                        // we have a block scope in the stmt_id
                        self.analyze_stmt(stmt_id, curr_block_context);
                        // TODO: if we have no terminators yet (didnt find a break or continue or
                        // return), then we add the default terminator (which goes to if_end_0)
                    },
                    None => {
                        /* if_cond_0:
                               branch 1 < 20, if_block_0, if_end_0;
                               if_block_0:
                                   jump end_if_0, (69, 420);
                           if_end_0:
                               return (4, 6);
                        */
                        let if_terminator = self.arena.alloc_terminator(TerminatorInst::Branch {
                            source_info,
                            cond,
                            true_block: if_body_block,
                            false_block: if_end_block,
                        });
                    },
                }
            },
            Statement::While { cond, body } => {
                //
            },
            Statement::StackBlock(expr_ids) => {
                //
            },
            Statement::BlockScope(top_level_ids, scope_kind) =>
                for top_level_id in top_level_ids.clone() {
                    match top_level_id {
                        TopLevelId::VariableDecl(var_id) => {
                            let var_inst = self.analyze_variable_declaration(var_id, curr_block_context);
                            self.arena.get_block_mut(curr_block_context.curr_block).instructions.push(var_inst)
                        },
                        TopLevelId::FunctionDecl(func_id) => panic!("[internal error] local functions are not allowed"),
                        TopLevelId::Statement(stmt_id) => self.analyze_stmt(stmt_id, curr_block_context),
                    }
                },
            Statement::Return => {
                // note that we have already checked that the state of the stack is correct in
                // terms of size and types before each return statement.
                // what the cfg_analyzer does is also check all branches against each other.
                let return_tuple = self.arena.alloc_inst(Instruction::Tuple {
                    source_info: source_info.clone(),
                    instructions: self.arena.hfs_stack.clone(),
                });
                let terminator = self.arena.alloc_terminator(TerminatorInst::Return(source_info, return_tuple));
                self.arena.get_block_mut(curr_block_context.curr_block).terminator = terminator;
            },
            Statement::Break => {
                // a break always ends the current block and jumps somewhere else
                // (the exit of the current control flow construct)
                let term = self.arena.alloc_terminator(TerminatorInst::Jump(source_info, curr_block_context.exit_block, None));
                self.arena.get_block_mut(curr_block_context.curr_block).terminator = term;
            },
            Statement::Continue => {
                // the entry_block is meant to be the block that we came from to start this current
                // context. its meant to allow the start of the next iteration
                let term = self.arena.alloc_terminator(TerminatorInst::Jump(source_info, curr_block_context.entry_block, None));
                self.arena.get_block_mut(curr_block_context.curr_block).terminator = term;
            },
            Statement::Empty => {},
            Statement::Assignment { value, identifier, is_move } => {
                // @(213) &= var; // move assignment
                // @(213) := var; // copy assignment
            },
            Statement::FunctionCall { args, identifier, is_move } => {
                // @(213) &> func; // move call
                // @(213) :> func; // copy call
            },
        }
    }
    fn analyze_expr(&mut self, id: ExprId, curr_block_context: BlockContext) -> InstId {
        todo!()
    }
}

// Debug printing functions (using the MIR syntax)
impl CfgAnalyzer {
    pub fn print_hfs_mir(&self, top_level_nodes: Vec<CfgTopLevelId>) {
        for id in top_level_nodes {
            match id {
                CfgTopLevelId::GlobalVarDecl(source_info, var_id) => {
                    let var = self.arena.get_var(var_id);
                    println!("{}", var.get_repr(&self.arena));
                },
                CfgTopLevelId::FunctionDecl(source_info, func_id) => {
                    let func = self.arena.get_func(func_id);
                    println!("{}", func.get_repr(&self.arena));
                },
            }
        }
    }
}
