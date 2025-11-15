use crate::hfs::{ast::*, Literal};
/*
=================================================================================================
Controlflow Graph IR Pass (HFS Medium IR):
    Check CFG-to-LLVM-IR-example.md/ll for a detailed example
=================================================================================================
IR that is built from the AST that includes CFG information and flattens control flow constructs
 this IR doesn't know that the HFS Stack exists at all. there is no StackBlock here.
 this is because everyone that wanted a given stack value has been given that ExprId in the AST. 
LLVM also doesn't care either, in their SSA an 'add' is also its own register
So we would generate it without really caring who uses it too.
 (though we must take that register and use it for the next computation, but its isolated much
 like our stack expressions were in henceforth. at this point, henceforth doesn't exist as much)

The key here is that we dont care about the AST's Statement nodes after this pass.
 and there isn't a Tree structure anymore either. CFG dependencies are established and each block 
 knows where to go next (its a graph). 

*/

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct BlockId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct InstId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct TermInstId(pub usize);

pub struct CfgFunction {
    identifier: FuncId,
    // we repeat the FunctionDeclaration methods because we are meant convert everything over
    // rather than keep acessing the old FunctionDeclaration (we still keep the FuncId though)
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
