use crate::hfs::ast::*;
/*
=================================================================================================
Controlflow Graph IR Pass
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

struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}
// each Instruction -> one value 
// even if we return many values when we leave a function
// we can think of it as returning one tuple (llvm also wants this anyways)
// and its annoying to make an API that wants to support Vec<ExprId>,
// but everything else wants individual ExprId
pub enum Instruction {
    VariableAssign {
        value: ExprId,
        identifier: Identifier, // GlobalVar or Variable
        is_move: bool,
    },
    FunctionCall {
        args: Vec<ExprId>,
        identifier: FuncId,
        is_move: bool,
    },
    Phi(Vec<(BlockId, ExprId)>),
    // note we basically move stack keywords outside the stack block
    // @( 1 2 3 @pop @pop 4) -----> @(1 2 3) @pop @pop @(4)
    // we assume stack keyword calls are exactly the same as function calls
    // but just have the Type system agnostic semantics
    // we convert StackKeyword Expressions to this
    StackKeywordCall {
        args: Vec<ExprId>,
        name: String,
        return_values: Vec<ExprId>,
    }
}

pub enum Terminator {
    Return(Vec<ExprId>), // only the last block in a function should have a Terminator::Return
    // all others that want to return should jump to the "end" block which is tracked by each
    // function
    Branch { cond: ExprId, true_block: BlockId, false_block: BlockId },
    Jump(BlockId),
    Unreachable,
}

/*

fn foo: (i32 i32 str) -> (i32 i32 i32) {
  start:
    let var: f32;
    assign var, 3.0; // @(3.0) &= var;
    branch var < 2.0, if_block_0, else_block_0;
    if_block_0:
        push
    else_block_0:
  end:
    let 0: i32;
    phi (if_block_0, 69), (else_block_0, 420)
    let 1: i32;
    let 2: i32;
    return 
}


*/

