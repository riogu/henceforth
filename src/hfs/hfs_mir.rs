use std::fmt::{format, Display};

use crate::hfs::{ast::*, InstArena, Literal};
/*
// =================================================================================================
// Control Flow Graph IR Pass (HFS MIR - Medium-level IR)
//     See HFS-MIR-to-LLVM-IR-example.md for an example
// =================================================================================================
// MIR is a graph-based IR that represents flattened control flow.
//
// Key properties:
// - Stack semantics eliminated: all values are explicitly named (InstId)
// - Control flow linearized: if/while/etc become blocks + branches/jumps
// - SSA-friendly: each instruction produces a single named value (if there is a value)
// - Graph structure: blocks connected by explicit control flow edges
// - Statement-free: AST's Statement nodes are entirely eliminated and turned into instructions
//
// This IR maps cleanly to LLVM IR and is ready for lowering or interpretation.
*/

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TermInstId(pub usize);

#[derive(Debug)]
pub struct CfgFunction {
    identifier: FuncId,
    pub name: String,
    // we repeat the FunctionDeclaration methods because we are meant convert everything over
    // rather than keep acessing the old FunctionDeclaration (we still keep the FuncId though)
    // also you can just store each CfgFunction with their FuncId too in the new arena
    pub param_type: TypeId,
    pub return_type: TypeId,

    pub parameter_exprs: Vec<InstId>,

    pub entry_block: BlockId,
    pub body_blocks: Vec<BasicBlock>,
    pub exit_block: BlockId,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub id: BlockId,
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub terminator: TerminatorInst,
    pub predecessors: Vec<BlockId>, // or phi node construction
}

#[derive(Debug)]
pub enum VarIdentifier {
    GlobalVar(VarId),
    Variable(VarId),
}

// each Instruction -> one value (this is just like LLVM SSA)
// but not all instructions produce values (declarations, stores, etc)
#[derive(Debug)]
pub enum Instruction {
    Parameter {
        index: usize,
        ty: TypeId,
    },
    VarDeclaration(VarId),
    Store {
        value: InstId,
        identifier: VarIdentifier, // GlobalVar or Variable
        is_move: bool,
    },
    FunctionCall {
        args: Vec<InstId>,
        identifier: FuncId,
        is_move: bool,
    },

    Phi {
        incoming: Vec<(BlockId, InstId)>, // (predecessor block, value)
    },

    // note we basically move stack keywords outside the stack block
    // @( 1 2 3 @pop @pop 4) -----> @(1 2 3) @pop @pop @(4)
    // we assume stack keyword calls are exactly the same as function calls
    // but just have the Type system agnostic semantics
    // we convert StackKeyword Expressions to this
    StackKeyword {
        name: String,
        args: Vec<InstId>,
    },
    Tuple {
        instructions: Vec<InstId>,
    },
    Push(Vec<InstId>),
    Operation(CfgOperation),
    Identifier(VarIdentifier),
    Literal(Literal),
}

// Terminator instructions, separated from the others
#[derive(Debug)]
pub enum TerminatorInst {
    Return(InstId), // only the last block in a function should have a Terminator::Return
    // all others that want to return should jump to the "end" block which is tracked by each function
    Branch {
        cond: InstId,
        true_block: BlockId,
        false_block: BlockId,
    },
    // if we want to jump with nothing, just have an empty vector
    Jump(BlockId, Vec<InstId>),
    Unreachable, // might be useful for you later in CFG analysis
                 // if you dont find it useful (you just wanna delete nodes instead) then remove this
}

#[derive(Debug, Clone, Copy)]
pub enum CfgOperation {
    Add(InstId, InstId),
    Sub(InstId, InstId),
    Mul(InstId, InstId),
    Div(InstId, InstId),
    Mod(InstId, InstId),
    Equal(InstId, InstId),
    NotEqual(InstId, InstId),
    Less(InstId, InstId),
    LessEqual(InstId, InstId),
    Greater(InstId, InstId),
    GreaterEqual(InstId, InstId),
    Or(InstId, InstId),
    And(InstId, InstId),
    Not(InstId),
}

// Debug printing trait and implementations
fn get_repr_many<T: CfgPrintable>(items: &Vec<T>, arena: &InstArena, sep: &str) -> String {
    items
        .iter()
        .map(|item| item.get_repr(arena))
        .collect::<Vec<String>>()
        .join(sep)
}

pub trait CfgPrintable {
    fn get_repr(&self, arena: &InstArena) -> String;
}

impl CfgPrintable for Type {
    fn get_repr(&self, arena: &InstArena) -> String {
        match self {
            Type::Int => String::from("i32"),
            Type::String => String::from("str"),
            Type::Bool => String::from("bool"),
            Type::Float => String::from("f32"),
            Type::Tuple(type_ids) => {
                // ID -> Type -> string representation
                let type_reprs: Vec<String> = type_ids
                    .iter()
                    .map(|id| arena.get_type(*id))
                    .map(|typ| typ.get_repr(arena))
                    .collect();

                type_reprs.join(" ")
            }
        }
    }
}

impl CfgPrintable for CfgFunction {
    fn get_repr(&self, arena: &InstArena) -> String {
        let params = arena.get_type(self.param_type).clone();
        let returns = arena.get_type(self.return_type).clone();
        format!(
            "fn {}: ({}) -> ({}) {{\n
                {}\n   
             }}",
            self.name,
            params.get_repr(arena),
            get_repr_many(&self.body_blocks, arena, "\n"),
            returns.get_repr(arena)
        )
    }
}

impl CfgPrintable for TerminatorInst {
    fn get_repr(&self, arena: &InstArena) -> String {
        match self {
            TerminatorInst::Return(inst_id) => {
                let inst = arena.get_instruction(*inst_id);
                format!("return {};", inst.get_repr(arena))
            }
            TerminatorInst::Branch {
                cond,
                true_block,
                false_block,
            } => {
                let cond = arena.get_instruction(*cond);
                let true_block = arena.get_block(*true_block);
                let false_block = arena.get_block(*false_block);
                format!(
                    "branch {}, {}, {};",
                    cond.get_repr(arena),
                    true_block.get_repr(arena),
                    false_block.get_repr(arena)
                )
            }
            TerminatorInst::Jump(block_id, inst_ids) => todo!(),
            TerminatorInst::Unreachable => String::from("unreachable;"),
        }
    }
}

impl CfgPrintable for Instruction {
    fn get_repr(&self, arena: &InstArena) -> String {
        match self {
            Instruction::Parameter { index, ty } => todo!(),
            Instruction::VarDeclaration(var_id) => {
                let var = arena.get_var(var_id);
            }
            Instruction::Store {
                value,
                identifier,
                is_move,
            } => todo!(),
            Instruction::FunctionCall {
                args,
                identifier,
                is_move,
            } => todo!(),
            Instruction::Phi { incoming } => todo!(),
            Instruction::StackKeyword { name, args } => todo!(),
            Instruction::Tuple { instructions } => todo!(),
            Instruction::Push(inst_ids) => todo!(),
            Instruction::Operation(cfg_operation) => todo!(),
            Instruction::Identifier(var_identifier) => todo!(),
            Instruction::Literal(literal) => todo!(),
        }
    }
}

impl CfgPrintable for BasicBlock {
    fn get_repr(&self, arena: &InstArena) -> String {
        let non_term_inst_repr = get_repr_many(&self.instructions, arena, "\n");
        let terminator_repr = self.terminator.get_repr(arena);
        format!("{}\n{}", non_term_inst_repr, terminator_repr)
    }
}
