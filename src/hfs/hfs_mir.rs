use crate::hfs::{ast::*, Literal};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct BlockId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct InstId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct TermInstId(pub usize);

pub struct CfgFunction {
    identifier: FuncId,
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

struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: TerminatorInst,
    pub predecessors: Vec<BlockId>, // or phi node construction
}

pub enum VarIdentifier {
    GlobalVar(VarId),
    Variable(VarId),
}

// each Instruction -> one value (this is just like LLVM SSA)
// but not all instructions produce values (declarations, stores, etc)
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
        incoming: Vec<(BlockId, InstId)>,  // (predecessor block, value)
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
pub enum TerminatorInst {
    Return(Vec<InstId>), // only the last block in a function should have a Terminator::Return
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
