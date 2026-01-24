use std::fmt::{Display, format};

use crate::hfs::{InstArena, Literal, SourceInfo, ast::*};
/*
=================================================================================================
Control Flow Graph IR Pass (HFS MIR - Medium-level IR)
    See HFS-MIR-to-LLVM-IR-example.md for an example
=================================================================================================
*/

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TermInstId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CfgTopLevelId {
    VariableDecl(SourceInfo, VarId),
    FunctionDecl(SourceInfo, FuncId),
}
#[derive(Debug)]
pub struct CfgFunction {
    pub old_func_id: FuncId, // i dont think we will wanna keep this
    // but for now its here (probs delete later)
    pub source_info: SourceInfo,
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
    GlobalVar(SourceInfo, VarId),
    Variable(SourceInfo, VarId),
}

// each Instruction -> one value (this is just like LLVM SSA)
// but not all instructions produce values (declarations, stores, etc)
#[derive(Debug)]
pub enum Instruction {
    Parameter {
        source_info: SourceInfo,
        index: usize,
        ty: TypeId,
    },
    VarDeclaration(SourceInfo, VarId),
    Store {
        source_info: SourceInfo,
        value: InstId,
        identifier: VarIdentifier, // GlobalVar or Variable
        is_move: bool,
    },
    FunctionCall {
        source_info: SourceInfo,
        args: Vec<InstId>,
        identifier: FuncId,
        is_move: bool,
    },

    Phi {
        source_info: SourceInfo,
        incoming: Vec<BlockId>, // (predecessor block, value)
    },

    // note we basically move stack keywords outside the stack block
    // @( 1 2 3 @pop @pop 4) -----> @(1 2 3) @pop @pop @(4)
    // we assume stack keyword calls are exactly the same as function calls
    // but just have the Type system agnostic semantics
    // we convert StackKeyword Expressions to this
    StackKeyword {
        source_info: SourceInfo,
        name: String,
        args: Vec<InstId>,
    },
    Tuple {
        source_info: SourceInfo,
        instructions: Vec<InstId>,
    },
    Push(SourceInfo, InstId), // always a tuple
    Operation(SourceInfo, CfgOperation),
    Identifier(SourceInfo, VarIdentifier),
    Literal(SourceInfo, Literal),
}

// Terminator instructions, separated from the others
#[derive(Debug)]
pub enum TerminatorInst {
    Return(SourceInfo, InstId), // only the last block in a function should have a Terminator::Return
    // all others that want to return should jump to the "end" block which is tracked by each function
    Branch { source_info: SourceInfo, cond: InstId, true_block: BlockId, false_block: BlockId },
    // if we want to jump with nothing, just have an empty vector
    Jump(BlockId, Option<InstId>), // is a tuple
    Unreachable,                   // might be useful for you later in CFG analysis
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
    items.iter().map(|item| item.get_repr(arena)).collect::<Vec<String>>().join(sep)
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
                let type_reprs: Vec<String> =
                    type_ids.iter().map(|id| arena.get_type(*id)).map(|typ| typ.get_repr(arena)).collect();

                type_reprs.join(" ")
            },
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
            returns.get_repr(arena),
            get_repr_many(&self.body_blocks, arena, "\n"),
        )
    }
}

impl CfgPrintable for VarDeclaration {
    fn get_repr(&self, arena: &InstArena) -> String {
        let typ = arena.get_type(self.hfs_type);
        format!("let {}: {};", self.name, typ.get_repr(arena))
    }
}

impl CfgPrintable for CfgOperation {
    fn get_repr(&self, arena: &InstArena) -> String {
        match self {
            CfgOperation::Add(inst_id1, inst_id2) => format!(
                "{} + {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::Sub(inst_id1, inst_id2) => format!(
                "{} - {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::Mul(inst_id1, inst_id2) => format!(
                "{} * {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::Div(inst_id1, inst_id2) => format!(
                "{} / {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::Mod(inst_id1, inst_id2) => format!(
                "{} % {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::Equal(inst_id1, inst_id2) => format!(
                "{} == {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::NotEqual(inst_id1, inst_id2) => format!(
                "{} != {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::Less(inst_id1, inst_id2) => format!(
                "{} < {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::LessEqual(inst_id1, inst_id2) => format!(
                "{} <= {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::Greater(inst_id1, inst_id2) => format!(
                "{} > {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::GreaterEqual(inst_id1, inst_id2) => format!(
                "{} >= {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::Or(inst_id1, inst_id2) => format!(
                "{} && {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::And(inst_id1, inst_id2) => format!(
                "{} && {}",
                arena.get_instruction(*inst_id1).get_repr(arena),
                arena.get_instruction(*inst_id2).get_repr(arena)
            ),
            CfgOperation::Not(inst_id) => format!("!{}", arena.get_instruction(*inst_id).get_repr(arena)),
        }
    }
}

impl CfgPrintable for TerminatorInst {
    fn get_repr(&self, arena: &InstArena) -> String {
        match self {
            TerminatorInst::Return(source_info, inst_id) => {
                let inst = arena.get_instruction(*inst_id);
                format!("return {};", inst.get_repr(arena))
            },
            TerminatorInst::Branch { source_info, cond, true_block, false_block } => {
                let cond = arena.get_instruction(*cond);
                let true_block = arena.get_block(*true_block);
                let false_block = arena.get_block(*false_block);
                format!("branch {}, {}, {};", cond.get_repr(arena), true_block.name, false_block.name)
            },
            TerminatorInst::Jump(block_id, inst_id) => {
                let block = arena.get_block(*block_id);
                match inst_id {
                    Some(id) => {
                        let inst = arena.get_instruction(*id);
                        format!("jump {}, {};", block.name, inst.get_repr(arena))
                    },
                    None => format!("jump {};", block.name),
                }
            },
            TerminatorInst::Unreachable => String::from("unreachable;"),
        }
    }
}

impl CfgPrintable for Instruction {
    fn get_repr(&self, arena: &InstArena) -> String {
        match self {
            Instruction::Parameter { source_info, index, ty } => todo!(),
            Instruction::VarDeclaration(source_info, var_id) => {
                let var = arena.get_var(*var_id);
                let var_type = arena.get_type(var.hfs_type);

                format!("let {}: {};", var.name, var_type.get_repr(arena))
            },
            Instruction::Store { source_info, value, identifier, is_move } => {
                let id = match identifier {
                    VarIdentifier::GlobalVar(source_info, var_id) => var_id,
                    VarIdentifier::Variable(source_info, var_id) => var_id,
                };
                let var = arena.get_var(*id);
                let value = arena.get_instruction(*value);

                format!("store {}, {};", var.name, value.get_repr(arena))
            },
            Instruction::FunctionCall { source_info, args, identifier, is_move } => {
                let func = arena.get_func(*identifier);
                let args_repr: Vec<String> =
                    args.iter().map(|id| arena.get_instruction(*id)).map(|inst| inst.get_repr(arena)).collect();
                format!("call {}, {};", func.name, args_repr.join(", "))
            },
            Instruction::Phi { source_info, incoming } => {
                let incoming: Vec<String> =
                    incoming.iter().map(|id| arena.get_block(*id)).map(|block| block.name.clone()).collect();
                format!("phi {};", incoming.join(", "))
            },
            Instruction::StackKeyword { source_info, name, args } => {
                let args_repr: Vec<String> =
                    args.iter().map(|id| arena.get_instruction(*id)).map(|inst| inst.get_repr(arena)).collect();
                format!("keyword {}, {};", name, args_repr.join(", "))
            },
            Instruction::Tuple { source_info, instructions } => {
                let instructions_repr: Vec<String> =
                    instructions.iter().map(|id| arena.get_instruction(*id)).map(|inst| inst.get_repr(arena)).collect();
                format!("({})", instructions_repr.join(", "))
            },
            Instruction::Push(source_info, inst_id) => {
                let inst = arena.get_instruction(*inst_id);
                format!("push {};", inst.get_repr(arena))
            },
            Instruction::Operation(source_info, cfg_operation) => cfg_operation.get_repr(arena),
            Instruction::Identifier(source_info, var_identifier) => {
                let id = match var_identifier {
                    VarIdentifier::GlobalVar(source_info, var_id) => var_id,
                    VarIdentifier::Variable(source_info, var_id) => var_id,
                };
                let var_decl = arena.get_var(*id);
                var_decl.name.clone()
            },
            Instruction::Literal(source_info, literal) => match literal {
                Literal::Integer(lit) => lit.to_string(),
                Literal::Float(lit) => lit.to_string(),
                Literal::String(lit) => lit.clone(),
                Literal::Bool(lit) => lit.to_string(),
            },
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
