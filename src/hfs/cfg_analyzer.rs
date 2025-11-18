use std::fmt::Debug;

use crate::hfs::{
    self, BasicBlock, BlockId, CfgFunction, CfgPrintable, InstId, Instruction, Literal, TermInstId, TerminatorInst, ast::*,
};
// here is where youll create the CFG pass and the new IR generation

#[derive(Debug)]
pub struct InstArena {
    pub vars: Vec<VarDeclaration>,
    pub functions: Vec<CfgFunction>,
    pub types: Vec<Type>,
    pub instructions: Vec<Instruction>,
    pub terminators: Vec<TerminatorInst>,
    pub blocks: Vec<BasicBlock>,
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
}

#[derive(Debug)]
pub struct CfgAnalyzer {
    pub arena: InstArena,
    // similar to StackAnalyzer, pretty much that
}

// Debug printing functions (using the MIR syntax)
impl CfgAnalyzer {
    pub fn print_hfs_mir(&self) {
        for func in &self.arena.functions {
            println!("{}", func.get_repr(&self.arena));
        }
    }
}
