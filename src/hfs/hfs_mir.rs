use std::{
    collections::HashMap,
    fmt::{format, Display},
};

use colored::{ColoredString, Colorize, CustomColor};

use crate::hfs::{ast::*, IrArena, Literal, SourceInfo};
/*
=================================================================================================
Control Flow Graph IR Pass (HFS MIR - Medium-level IR)
    See HFS-MIR-to-LLVM-IR-example.md for an example
=================================================================================================
*/

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct BlockId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct InstId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TermInstId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct IrFuncId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct GlobalIrVarId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CfgTopLevelId {
    GlobalVarDecl(GlobalIrVarId),
    FunctionDecl(IrFuncId),
}
#[derive(Debug)]
pub struct CfgFunction {
    pub source_info: SourceInfo,
    pub name: String,
    // we repeat the FunctionDeclaration methods because we are meant convert everything over
    // rather than keep acessing the old FunctionDeclaration (we still keep the FuncId though)
    // also you can just store each CfgFunction with their FuncId too in the new arena
    pub param_type: TypeId,
    pub return_type: TypeId,

    pub parameter_insts: Vec<InstId>,
    pub entry_block: BlockId, // CFG of blocks
}

#[derive(Debug)]
pub struct BasicBlock {
    pub parent_function: IrFuncId,
    pub name: String,
    pub predecessors: Vec<BlockId>, // or phi node construction
    pub instructions: Vec<InstId>,
    pub terminator: Option<TermInstId>,
}

// Declarations (shared)
#[derive(Debug)]
pub struct GlobalIrVarDeclaration {
    pub source_info: SourceInfo,
    pub name: String,
    pub hfs_type: TypeId,
}
// each Instruction -> one value (this is just like LLVM SSA)
// but not all instructions produce values (declarations, stores, etc)
#[derive(Debug, Clone)]
pub enum Instruction {
    Load {
        source_info: SourceInfo,
        address: InstId,
        type_id: TypeId,
    },
    Store {
        source_info: SourceInfo,
        address: InstId,
        value: InstId,
    },
    Alloca {
        source_info: SourceInfo,
        type_id: TypeId,
    },
    GlobalAlloca(GlobalIrVarId),
    // this instruction doesnt "exist" because its only present in the global namespace
    // please print the variable it holds and if you want annotate it like llvm with an alloca
    // @mycool_global_var = global i32 69420
    Parameter {
        source_info: SourceInfo,
        index: usize,
        type_id: TypeId,
    },
    ReturnValue {
        // this is the temporary representation of the result of a function call as "local values"
        // we just store the type, the value is mapped at runtime by the interpreter
        source_info: SourceInfo,
        type_id: TypeId,
    },
    FunctionCall {
        source_info: SourceInfo,
        args: Vec<InstId>,
        func_id: IrFuncId,
        is_move: bool,
        return_values: Vec<InstId>,
    },

    Phi {
        source_info: SourceInfo,
        incoming: HashMap<BlockId, InstId>, // (predecessor block, value from that block)
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
    Operation(SourceInfo, CfgOperation),
    Literal(SourceInfo, Literal),
    LoadElement {
        // load an element from a tuple (used to merge tuples into a new one)
        // useful for mapping our stack tracking into LLVM IR
        source_info: SourceInfo,
        index: usize,
        tuple: InstId,
    },
}
impl Instruction {
    pub(crate) fn get_source_info(&self) -> SourceInfo {
        match self {
            Instruction::Load { source_info, address, type_id } => source_info.clone(),
            Instruction::Store { source_info, address, value } => source_info.clone(),
            Instruction::Alloca { source_info, type_id } => source_info.clone(),
            Instruction::GlobalAlloca(global_ir_var_id) => todo!(),
            Instruction::Parameter { source_info, index, type_id } => source_info.clone(),
            Instruction::ReturnValue { source_info, type_id } => source_info.clone(),
            Instruction::FunctionCall { source_info, args, func_id, is_move, return_values } => source_info.clone(),
            Instruction::Phi { source_info, incoming } => source_info.clone(),
            Instruction::StackKeyword { source_info, name, args } => source_info.clone(),
            Instruction::Tuple { source_info, instructions } => source_info.clone(),
            Instruction::Operation(source_info, cfg_operation) => source_info.clone(),
            Instruction::Literal(source_info, literal) => source_info.clone(),
            Instruction::LoadElement { source_info, index, tuple } => source_info.clone(),
        }
    }
}

// Terminator instructions, separated from the others
#[derive(Debug)]
pub enum TerminatorInst {
    Return(SourceInfo, InstId), // only the last block in a function should have a Terminator::Return
    // all others that want to return should jump to the "end" block which is tracked by each function
    Branch { source_info: SourceInfo, cond: InstId, true_block: BlockId, false_block: BlockId },
    // if we want to jump with nothing, just have an empty vector
    Jump(SourceInfo, BlockId), // is a tuple
    // the jump always carries around the stack variation itself
    // for other purposes, we usually generate a phi and find the associated InstId with a pass
    // that searches for store instructions and whatnot
    Unreachable, // might be useful for you later in CFG analysis
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
fn get_repr_many<T: CfgPrintable>(items: &Vec<T>, arena: &IrArena, sep: &str) -> String {
    items.iter().map(|item| item.get_repr(arena).to_string()).collect::<Vec<String>>().join(sep)
}

pub trait CfgPrintable {
    fn get_repr(&self, arena: &IrArena) -> ColoredString;
}

impl CfgPrintable for Type {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        match self {
            Type::Int { .. } => String::from("i32").yellow(),
            Type::String { .. } => String::from("str").yellow(),
            Type::Bool { .. } => String::from("bool").yellow(),
            Type::Float { .. } => String::from("f32").yellow(),
            Type::Tuple { type_ids, .. } => {
                // ID -> Type -> string representation
                let type_reprs: Vec<String> =
                    type_ids.iter().map(|id| arena.get_type(*id)).map(|typ| typ.get_repr(arena).to_string()).collect();

                type_reprs.join(" ").into()
            },
        }
    }
}

impl CfgFunction {
    fn collect_blocks<'a>(&'a self, arena: &'a IrArena) -> Vec<&'a BasicBlock> {
        let mut visited = std::collections::HashSet::new();
        let mut worklist = vec![self.entry_block];
        let mut blocks = Vec::new();

        while let Some(block_id) = worklist.pop() {
            if !visited.insert(block_id) {
                continue;
            }

            let curr_block = arena.get_block(block_id);
            blocks.push(curr_block);

            match curr_block.terminator {
                Some(id) => match arena.get_terminator_instruction(id) {
                    TerminatorInst::Return(source_info, inst_id) => {},
                    TerminatorInst::Branch { source_info, cond, true_block, false_block } => {
                        worklist.push(*false_block);
                        worklist.push(*true_block);
                    },
                    TerminatorInst::Jump(source_info, block_id) => {
                        worklist.push(*block_id);
                    },
                    TerminatorInst::Unreachable => {},
                },
                None => {
                    // panic!("[internal error] tried collecting blocks with no terminator")
                },
            }
        }

        blocks
    }
}

impl CfgPrintable for CfgFunction {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        let params = arena.get_type(self.param_type).clone();
        let returns = arena.get_type(self.return_type).clone();
        let blocks_repr = self
            .collect_blocks(arena)
            .iter()
            .map(|block| block.get_repr(arena).to_string())
            .collect::<Vec<String>>()
            .join("\n");
        format!(
            "{} {}{} {}{}{} {} {}{}{} {}\n{}\n{}",
            "fn".purple(),
            self.name.blue(),
            ":".custom_color(CustomColor::new(129, 137, 150)),
            "(".custom_color(CustomColor::new(129, 137, 150)),
            params.get_repr(arena),
            ")".custom_color(CustomColor::new(129, 137, 150)),
            "->".bright_blue(),
            "(".custom_color(CustomColor::new(129, 137, 150)),
            returns.get_repr(arena),
            ")".custom_color(CustomColor::new(129, 137, 150)),
            "{".custom_color(CustomColor::new(129, 137, 150)),
            blocks_repr,
            "}".custom_color(CustomColor::new(129, 137, 150)),
        )
        .into()
    }
}

impl CfgPrintable for GlobalIrVarDeclaration {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        let typ = arena.get_type(self.hfs_type);
        format!(
            "{} {}{} {}{}",
            "let".purple(),
            self.name,
            ":".custom_color(CustomColor::new(129, 137, 150)),
            typ.get_repr(arena),
            ";".custom_color(CustomColor::new(129, 137, 150))
        )
        .into()
    }
}

impl CfgPrintable for CfgOperation {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        let op = match self {
            CfgOperation::Add(a, b) => format!("{} {} {}", arena.inst_name(*a), "+".bright_blue(), arena.inst_name(*b)),
            CfgOperation::Sub(a, b) => format!("{} {} {}", arena.inst_name(*a), "-".bright_blue(), arena.inst_name(*b)),
            CfgOperation::Mul(a, b) => format!("{} {} {}", arena.inst_name(*a), "*".bright_blue(), arena.inst_name(*b)),
            CfgOperation::Div(a, b) => format!("{} {} {}", arena.inst_name(*a), "/".bright_blue(), arena.inst_name(*b)),
            CfgOperation::Mod(a, b) => format!("{} {} {}", arena.inst_name(*a), "%".bright_blue(), arena.inst_name(*b)),
            CfgOperation::Equal(a, b) => format!("{} {} {}", arena.inst_name(*a), "==".bright_blue(), arena.inst_name(*b)),
            CfgOperation::NotEqual(a, b) => format!("{} {} {}", arena.inst_name(*a), "!=".bright_blue(), arena.inst_name(*b)),
            CfgOperation::Less(a, b) => format!("{} {} {}", arena.inst_name(*a), "<".bright_blue(), arena.inst_name(*b)),
            CfgOperation::LessEqual(a, b) => format!("{} {} {}", arena.inst_name(*a), "<=".bright_blue(), arena.inst_name(*b)),
            CfgOperation::Greater(a, b) => format!("{} {} {}", arena.inst_name(*a), ">".bright_blue(), arena.inst_name(*b)),
            CfgOperation::GreaterEqual(a, b) => format!("{} {} {}", arena.inst_name(*a), ">=".bright_blue(), arena.inst_name(*b)),
            CfgOperation::Or(a, b) => format!("{} {} {}", arena.inst_name(*a), "||".bright_blue(), arena.inst_name(*b)),
            CfgOperation::And(a, b) => format!("{} {} {}", arena.inst_name(*a), "&&".bright_blue(), arena.inst_name(*b)),
            CfgOperation::Not(a) => format!("{}{}", "!".bright_blue(), arena.inst_name(*a)),
        };
        op.into()
    }
}

impl CfgPrintable for TerminatorInst {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        match self {
            TerminatorInst::Return(_, inst_id) => format!("{} {}", "return".purple(), arena.inst_name(*inst_id)).into(),
            TerminatorInst::Branch { cond, true_block, false_block, .. } => format!(
                "{} {}, {}, {}",
                "branch".purple(),
                arena.inst_name(*cond),
                arena.get_block(*true_block).name,
                arena.get_block(*false_block).name
            )
            .into(),
            TerminatorInst::Jump(_, block_id) => format!("{} {}", "jump".purple(), arena.get_block(*block_id).name).into(),
            TerminatorInst::Unreachable => format!("{}", "unreachable".purple()).into(),
        }
    }
}

impl CfgPrintable for Instruction {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        match self {
            Instruction::Parameter { source_info, index, type_id } =>
                format!("{} %arg{}", arena.get_type(*type_id).get_repr(arena), index).into(),
            Instruction::FunctionCall { source_info, args, func_id, is_move, return_values } => {
                let func = arena.get_func(*func_id);
                let args_repr: Vec<String> = args.iter().map(|id| arena.inst_name(*id)).collect();
                format!("{} {}, {}", "call".purple(), func.name.blue(), args_repr.join(", ")).into()
            },
            Instruction::Phi { source_info, incoming } => {
                let incoming: Vec<String> = incoming
                    .iter()
                    .map(|(block_id, inst_id)| format!("[{}: {}]", arena.get_block(*block_id).name, arena.inst_name(*inst_id)))
                    .collect();
                format!("{} {}", "phi".purple(), incoming.join(", ")).into()
            },
            Instruction::Tuple { source_info, instructions } => {
                let reprs: Vec<String> = instructions.iter().map(|id| arena.inst_name(*id)).collect();
                format!(
                    "{}{}{}",
                    "(".custom_color(CustomColor::new(129, 137, 150)),
                    reprs.join(", "),
                    ")".custom_color(CustomColor::new(129, 137, 150))
                )
                .into()
            },
            Instruction::Operation(source_info, op) => op.get_repr(arena),
            Instruction::Literal(source_info, literal) => match literal {
                Literal::Integer(lit) => lit.to_string().green(),
                Literal::Float(lit) => lit.to_string().green(),
                Literal::String(lit) => format!("\"{}\"", lit).green(),
                Literal::Bool(lit) => lit.to_string().green(),
            },
            Instruction::LoadElement { source_info, index, tuple } =>
                format!("{} {}, {}", "load_element".purple(), format!("{}", index).green(), arena.inst_name(*tuple)).into(),
            Instruction::ReturnValue { source_info, type_id } =>
                format!("{} {}", "retval".purple(), arena.get_type(*type_id).get_repr(arena)).into(),
            Instruction::Load { source_info, address, type_id } =>
                format!("{} {}", "load".purple(), arena.inst_name(*address)).into(),
            Instruction::Store { source_info, address, value } =>
                format!("{} {}, {}", "store".purple(), arena.inst_name(*value), arena.inst_name(*address)).into(),
            Instruction::StackKeyword { source_info, name, args } => {
                let args_repr: Vec<String> = args.iter().map(|id| arena.inst_name(*id)).collect();
                format!("keyword {}, {}", name, args_repr.join(", ")).into()
            },
            Instruction::Alloca { source_info, type_id } => format!("{} {}", "alloca".purple(), arena.get_type(*type_id)).into(),
            Instruction::GlobalAlloca(global_ir_var_id) => todo!("joao pls pls help ;_;"),
        }
    }
}

impl CfgPrintable for BasicBlock {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        let instructions =
            self.instructions.iter().map(|inst| arena.get_instruction(*inst).clone()).collect::<Vec<Instruction>>();
        let non_term_inst_repr = self
            .instructions
            .iter()
            .map(|id| format!("    {} = {}", arena.inst_name(*id), arena.get_instruction(*id).get_repr(arena)))
            .collect::<Vec<String>>()
            .join("\n");
        let terminator_repr = match self.terminator {
            Some(id) => format!("    {}", arena.get_terminator_instruction(id).get_repr(arena)),
            None => String::from("    <no terminator>"),
        };
        let mut parts = vec![format!("  {}:", self.name.red())];
        if !non_term_inst_repr.is_empty() {
            parts.push(non_term_inst_repr);
        }
        parts.push(terminator_repr);
        parts.join("\n").into()
    }
}
impl IrArena {
    pub fn dump(&self, top_level: &Vec<CfgTopLevelId>) {
        eprintln!("total blocks: {}", self.blocks.len());
        for (i, block) in self.blocks.iter().enumerate() {
            eprintln!(
                "  block {} ({}): {} instructions, terminator: {}",
                i,
                block.name,
                block.instructions.len(),
                block.terminator.is_some()
            );
        }
        for node in top_level {
            match node {
                CfgTopLevelId::GlobalVarDecl(id) => println!("{}", self.get_var(*id).get_repr(self)),
                CfgTopLevelId::FunctionDecl(id) => println!("{}", self.get_func(*id).get_repr(self)),
            }
        }
    }
}
