use std::fmt::Debug;

use crate::hfs::{
    self, ast::*, BasicBlock, BlockId, CfgFunction, CfgPrintable, CfgTopLevelId, InstId, Instruction, Literal, SourceInfo,
    TermInstId, TerminatorInst, Token, TokenKind, PRIMITIVE_TYPE_COUNT,
};

// here is where youll create the CFG pass and the new IR generation

#[derive(Debug, Default)]
pub struct InstArena<'a> {
    pub vars: Vec<VarDeclaration>,
    pub functions: Vec<CfgFunction>,
    pub types: Vec<Type>,
    pub instructions: Vec<Instruction>,
    pub terminators: Vec<TerminatorInst>,
    pub blocks: Vec<BasicBlock>,

    pub var_tokens: Vec<Token<'a>>,
    pub function_tokens: Vec<Token<'a>>,
    pub type_tokens: Vec<Token<'a>>,
}
impl<'a> InstArena<'a> {
    pub fn new() -> Self {
        let mut arena = Self::default();
        arena.alloc_type_uncached(Type::Int, Token { kind: TokenKind::Int, source_info: SourceInfo::new(0, 0, 0, "Int") });
        arena.alloc_type_uncached(Type::Float, Token { kind: TokenKind::Float, source_info: SourceInfo::new(0, 0, 0, "Float") });
        arena.alloc_type_uncached(Type::Bool, Token { kind: TokenKind::Bool, source_info: SourceInfo::new(0, 0, 0, "Bool") });
        arena.alloc_type_uncached(Type::String, Token {
            kind: TokenKind::String,
            source_info: SourceInfo::new(0, 0, 0, "String"),
        });
        arena
    }

    fn alloc_type_uncached(&mut self, hfs_type: Type, token: Token<'a>) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type.clone());
        self.type_tokens.push(token);
        id
    }
}

impl<'a> InstArena<'a> {
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
}

#[derive(Debug)]
pub struct CfgAnalyzer<'a> {
    pub ast_arena: AstArena<'a>,
    pub arena: InstArena<'a>,
    // similar to StackAnalyzer, pretty much that
}

impl<'a> CfgAnalyzer<'a> {
    pub fn new(ast_arena: AstArena<'a>) -> Self {
        let mut arena = InstArena::new();
        arena.types.extend_from_slice(&ast_arena.types[PRIMITIVE_TYPE_COUNT..]);
        Self { ast_arena, arena }
    }

    pub fn analyze(top_level: Vec<TopLevelId>, ast_arena: AstArena<'a>) -> InstArena<'a> {
        let mut cfg_analyzer = CfgAnalyzer::new(ast_arena);
        let analyzed_top_level = cfg_analyzer.analyze_top_level(top_level);
        cfg_analyzer.print_hfs_mir(analyzed_top_level);
        cfg_analyzer.arena
    }

    fn analyze_function_declaration(&mut self, id: FuncId) -> FuncId {
        todo!()
    }

    fn analyze_top_level(&mut self, top_level: Vec<TopLevelId>) -> Vec<CfgTopLevelId> {
        let mut analyzed_nodes = Vec::<CfgTopLevelId>::new();
        for node in top_level.clone() {
            let new_node = match node {
                TopLevelId::VariableDecl(id) => CfgTopLevelId::VariableDecl(self.analyze_variable_declaration(id)),
                TopLevelId::FunctionDecl(id) => CfgTopLevelId::FunctionDecl(self.analyze_function_declaration(id)),
                TopLevelId::Statement(id) => todo!(),
            };
            analyzed_nodes.push(new_node);
        }
        analyzed_nodes
    }

    fn analyze_variable_declaration(&self, id: VarId) -> VarId {
        todo!()
    }
}

// Debug printing functions (using the MIR syntax)
impl<'a> CfgAnalyzer<'a> {
    pub fn print_hfs_mir(&self, top_level_nodes: Vec<CfgTopLevelId>) {
        for id in top_level_nodes {
            match id {
                CfgTopLevelId::VariableDecl(var_id) => {
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
