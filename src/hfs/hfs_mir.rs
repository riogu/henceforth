use std::{collections::HashSet, fmt::Display, vec};

use colored::{ColoredString, Colorize, CustomColor};
use indexmap::IndexMap;
use slotmap::new_key_type;

use crate::hfs::{ast::*, IrArena, Literal, SourceInfo};
/*
=================================================================================================
Control Flow Graph IR Pass (HFS MIR - Medium-level IR)
    See HFS-MIR-to-LLVM-IR-example.md for an example
=================================================================================================
*/

new_key_type! {
    pub struct InstId;
    pub struct TermInstId;
    pub struct BlockId;
    pub struct IrFuncId;
    pub struct GlobalIrVarId;
}
#[derive(PartialEq, Clone)]
pub enum InstOrTermId {
    InstId(InstId),
    TermInstId(TermInstId),
}
impl From<InstId> for InstOrTermId {
    fn from(id: InstId) -> Self {
        InstOrTermId::InstId(id)
    }
}

impl From<TermInstId> for InstOrTermId {
    fn from(id: TermInstId) -> Self {
        InstOrTermId::TermInstId(id)
    }
}
impl Display for InstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_ffi() as u32)
    }
}
impl Display for TermInstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_ffi() as u32)
    }
}
impl Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_ffi() as u32)
    }
}
impl Display for IrFuncId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_ffi() as u32)
    }
}
impl Display for GlobalIrVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_ffi() as u32)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IrTopLevelId {
    GlobalVarDecl(GlobalIrVarId),
    FunctionDecl(IrFuncId),
}
#[derive(Debug, Clone)]
pub struct IrFunction {
    pub source_info: SourceInfo,
    pub name: String,
    // we repeat the FunctionDeclaration methods because we are meant convert everything over
    // rather than keep acessing the old FunctionDeclaration (we still keep the FuncId though)
    // also you can just store each CfgFunction with their FuncId too in the new arena
    pub param_type: TypeId,
    pub return_type: TypeId,

    pub parameter_insts: Vec<InstId>,
    // note that you can get all blocks by insertion order from the IrArena
    pub entry_block: BlockId, // CFG of blocks
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub parent_function: IrFuncId,
    pub name: String,
    pub predecessors: HashSet<BlockId>, // for phi node construction
    pub successors: HashSet<BlockId>,
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
        incoming: IndexMap<BlockId, InstId>, // (predecessor block, value from that block)
    },

    // note we basically move stack keywords outside the stack block
    // @( 1 2 3 @pop @pop 4) -----> @(1 2 3) @pop @pop @(4)
    // we assume stack keyword calls are exactly the same as function calls
    // but just have the Type system agnostic semantics
    // we convert StackKeyword Expressions to this
    Tuple {
        source_info: SourceInfo,
        instructions: Vec<InstId>,
    },
    Operation {
        source_info: SourceInfo,
        op: IrOperation,
    },
    Literal {
        source_info: SourceInfo,
        literal: Literal,
    },
    LoadElement {
        // load an element from a tuple (used to merge tuples into a new one)
        // useful for mapping our stack tracking into LLVM IR
        source_info: SourceInfo,
        index: usize,
        tuple: InstId,
    },
}
// Terminator instructions, separated from the others
#[derive(Debug, Clone, Copy)]
pub enum TerminatorInst {
    Return { source_info: SourceInfo, return_tuple: InstId },
    Branch { source_info: SourceInfo, cond: InstId, true_block: BlockId, false_block: BlockId },
    // if we want to jump with nothing, just have an empty vector
    Jump { source_info: SourceInfo, target: BlockId }, // is a tuple
    // the jump always carries around the stack variation itself
    // for other purposes, we usually generate a phi and find the associated InstId with a pass
    // that searches for store instructions and whatnot
    Unreachable, // might be useful for you later in CFG analysis
}

impl Instruction {
    pub fn has_side_effects(&self) -> bool {
        match self {
            // function calls and memory ops are considered to have side effects
            // memory ops are only solved with something like mem2reg
            Instruction::FunctionCall { .. } | Instruction::Store { .. } | Instruction::Alloca { .. } => true,
            Instruction::Load { .. }
            | Instruction::GlobalAlloca(..)
            | Instruction::Parameter { .. }
            | Instruction::ReturnValue { .. }
            | Instruction::Phi { .. }
            | Instruction::Tuple { .. }
            | Instruction::Operation { .. }
            | Instruction::Literal { .. }
            | Instruction::LoadElement { .. } => false,
        }
    }
    pub fn replace_operands(&mut self, new_id: InstId, operand_idx: usize) {
        match self {
            Instruction::Store { address, value, .. } =>
                if operand_idx == 0 {
                    *address = new_id
                } else if operand_idx == 1 {
                    *value = new_id
                } else {
                    panic!("[internal error] passed wrong index to binary instruction")
                },
            Instruction::Load { address, .. } => *address = new_id,
            Instruction::FunctionCall { args, .. } => args[operand_idx] = new_id,
            Instruction::Phi { incoming, .. } => incoming[operand_idx] = new_id,
            Instruction::Tuple { instructions, .. } => instructions[operand_idx] = new_id,
            Instruction::LoadElement { tuple, .. } => *tuple = new_id,
            Instruction::Operation { op, .. } => match op {
                IrOperation::Add(inst_id, inst_id1)
                | IrOperation::Sub(inst_id, inst_id1)
                | IrOperation::Mul(inst_id, inst_id1)
                | IrOperation::Div(inst_id, inst_id1)
                | IrOperation::Mod(inst_id, inst_id1)
                | IrOperation::Equal(inst_id, inst_id1)
                | IrOperation::NotEqual(inst_id, inst_id1)
                | IrOperation::Less(inst_id, inst_id1)
                | IrOperation::LessEqual(inst_id, inst_id1)
                | IrOperation::Greater(inst_id, inst_id1)
                | IrOperation::GreaterEqual(inst_id, inst_id1)
                | IrOperation::Or(inst_id, inst_id1)
                | IrOperation::And(inst_id, inst_id1) =>
                    if operand_idx == 0 {
                        *inst_id = new_id
                    } else if operand_idx == 1 {
                        *inst_id1 = new_id
                    } else {
                        panic!("[internal error] passed wrong index to binary instruction")
                    },
                IrOperation::Not(inst_id) => *inst_id = new_id,
            },
            Instruction::Literal { .. }
            | Instruction::Alloca { .. }
            | Instruction::GlobalAlloca(_)
            | Instruction::Parameter { .. }
            | Instruction::ReturnValue { .. } => panic!("[internal error] can't replace operand of instruction with no operands"),
        }
    }
    pub fn get_operands(&self) -> Vec<InstId> {
        match self {
            Instruction::Store { address, value, .. } => vec![*address, *value],
            Instruction::Load { address, .. } => vec![*address],
            Instruction::FunctionCall { args, .. } => args.clone(),
            Instruction::Phi { incoming, .. } => incoming.values().copied().collect(),
            Instruction::Tuple { instructions, .. } => instructions.clone(),
            Instruction::LoadElement { tuple, .. } => vec![*tuple],
            Instruction::Operation { op, .. } => match op {
                IrOperation::Add(inst_id, inst_id1)
                | IrOperation::Sub(inst_id, inst_id1)
                | IrOperation::Mul(inst_id, inst_id1)
                | IrOperation::Div(inst_id, inst_id1)
                | IrOperation::Mod(inst_id, inst_id1)
                | IrOperation::Equal(inst_id, inst_id1)
                | IrOperation::NotEqual(inst_id, inst_id1)
                | IrOperation::Less(inst_id, inst_id1)
                | IrOperation::LessEqual(inst_id, inst_id1)
                | IrOperation::Greater(inst_id, inst_id1)
                | IrOperation::GreaterEqual(inst_id, inst_id1)
                | IrOperation::Or(inst_id, inst_id1)
                | IrOperation::And(inst_id, inst_id1) => {
                    vec![*inst_id, *inst_id1]
                },
                IrOperation::Not(inst_id) => vec![*inst_id],
            },
            Instruction::Literal { .. }
            | Instruction::Alloca { .. }
            | Instruction::GlobalAlloca(_)
            | Instruction::Parameter { .. }
            | Instruction::ReturnValue { .. } => vec![],
        }
    }
}
impl TerminatorInst {
    pub fn has_side_effects(&self) -> bool {
        // terminators always have side effects
        true
    }
    pub fn get_operands(&self) -> Vec<InstId> {
        match self {
            TerminatorInst::Return { return_tuple, .. } => vec![*return_tuple],
            TerminatorInst::Branch { cond, .. } => vec![*cond],
            TerminatorInst::Jump { .. } | TerminatorInst::Unreachable => vec![],
        }
    }
    pub fn replace_operand(&mut self, new_id: InstId) {
        match self {
            TerminatorInst::Return { return_tuple, .. } => *return_tuple = new_id,
            TerminatorInst::Branch { cond, .. } => *cond = new_id,
            TerminatorInst::Jump { .. } | TerminatorInst::Unreachable =>
                panic!("[internal error] can't replace operand of terminator with no operands"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IrOperation {
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

impl Instruction {
    pub(crate) fn get_source_info(&self) -> SourceInfo {
        match self {
            Instruction::Load { source_info, address: _, type_id: _ } => source_info.clone(),
            Instruction::Store { source_info, address: _, value: _ } => source_info.clone(),
            Instruction::Alloca { source_info, type_id: _ } => source_info.clone(),
            Instruction::GlobalAlloca(_) => todo!(),
            Instruction::Parameter { source_info, index: _, type_id: _ } => source_info.clone(),
            Instruction::ReturnValue { source_info, type_id: _ } => source_info.clone(),
            Instruction::FunctionCall { source_info, args: _, func_id: _, is_move: _, return_values: _ } => source_info.clone(),
            Instruction::Phi { source_info, incoming: _ } => source_info.clone(),
            Instruction::Tuple { source_info, instructions: _ } => source_info.clone(),
            Instruction::Operation { source_info, op: _ } => source_info.clone(),
            Instruction::Literal { source_info, literal: _ } => source_info.clone(),
            Instruction::LoadElement { source_info, index: _, tuple: _ } => source_info.clone(),
        }
    }
}

// Debug printing trait and implementations
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
impl CfgPrintable for IrFunction {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        let params = arena.get_type(self.param_type).clone();
        let returns = arena.get_type(self.return_type).clone();
        let func_id = arena.functions.iter().find(|(_, func)| func.name == self.name).unwrap().0;
        let blocks_repr = arena
            .compute_reverse_postorder(func_id)
            .iter()
            .map(|block| arena.get_block(*block).get_repr(arena).to_string())
            .collect::<Vec<String>>()
            .join("\n");
        format!(
            "{} {}{} {}{}{} {} {}{}{} {}\n{}\n{}",
            "fn".custom_color(CustomColor::new(202, 167, 244)),
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
            "let".custom_color(CustomColor::new(202, 167, 244)),
            self.name,
            ":".custom_color(CustomColor::new(129, 137, 150)),
            typ.get_repr(arena),
            ";".custom_color(CustomColor::new(129, 137, 150))
        )
        .into()
    }
}

impl CfgPrintable for IrOperation {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        let op = match self {
            IrOperation::Add(a, b) => format!("{} {} {}", arena.inst_name(*a), "+".bright_blue(), arena.inst_name(*b)),
            IrOperation::Sub(a, b) => format!("{} {} {}", arena.inst_name(*a), "-".bright_blue(), arena.inst_name(*b)),
            IrOperation::Mul(a, b) => format!("{} {} {}", arena.inst_name(*a), "*".bright_blue(), arena.inst_name(*b)),
            IrOperation::Div(a, b) => format!("{} {} {}", arena.inst_name(*a), "/".bright_blue(), arena.inst_name(*b)),
            IrOperation::Mod(a, b) => format!("{} {} {}", arena.inst_name(*a), "%".bright_blue(), arena.inst_name(*b)),
            IrOperation::Equal(a, b) => format!("{} {} {}", arena.inst_name(*a), "==".bright_blue(), arena.inst_name(*b)),
            IrOperation::NotEqual(a, b) => format!("{} {} {}", arena.inst_name(*a), "!=".bright_blue(), arena.inst_name(*b)),
            IrOperation::Less(a, b) => format!("{} {} {}", arena.inst_name(*a), "<".bright_blue(), arena.inst_name(*b)),
            IrOperation::LessEqual(a, b) => format!("{} {} {}", arena.inst_name(*a), "<=".bright_blue(), arena.inst_name(*b)),
            IrOperation::Greater(a, b) => format!("{} {} {}", arena.inst_name(*a), ">".bright_blue(), arena.inst_name(*b)),
            IrOperation::GreaterEqual(a, b) => format!("{} {} {}", arena.inst_name(*a), ">=".bright_blue(), arena.inst_name(*b)),
            IrOperation::Or(a, b) => format!("{} {} {}", arena.inst_name(*a), "||".bright_blue(), arena.inst_name(*b)),
            IrOperation::And(a, b) => format!("{} {} {}", arena.inst_name(*a), "&&".bright_blue(), arena.inst_name(*b)),
            IrOperation::Not(a) => format!("{}{}", "!".bright_blue(), arena.inst_name(*a)),
        };
        op.into()
    }
}

impl CfgPrintable for TerminatorInst {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        match self {
            TerminatorInst::Return { source_info: _, return_tuple: inst_id } => {
                format!("{} {}", "return".custom_color(CustomColor::new(202, 167, 244)), arena.inst_name(*inst_id)).into()
            },
            TerminatorInst::Branch { cond, true_block, false_block, .. } => format!(
                "{} {}, {}, {}",
                "branch".custom_color(CustomColor::new(202, 167, 244)),
                arena.inst_name(*cond),
                arena.get_block(*true_block).name,
                arena.get_block(*false_block).name
            )
            .into(),
            TerminatorInst::Jump { source_info: _, target: block_id } => {
                format!("{} {}", "jump".custom_color(CustomColor::new(202, 167, 244)), arena.get_block(*block_id).name).into()
            },
            TerminatorInst::Unreachable => format!("{}", "unreachable".custom_color(CustomColor::new(202, 167, 244))).into(),
        }
    }
}

impl CfgPrintable for Instruction {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        match self {
            Instruction::Parameter { source_info: _, index, type_id } => {
                format!("{} %arg{}", arena.get_type(*type_id).get_repr(arena), index).into()
            },
            Instruction::FunctionCall { source_info: _, args, func_id, is_move: _, return_values: _ } => {
                let func = arena.get_func(*func_id);
                let args_repr: Vec<String> = args.iter().map(|id| arena.inst_name(*id)).collect();
                format!("{} {}, {}", "call".custom_color(CustomColor::new(202, 167, 244)), func.name.blue(), args_repr.join(", "))
                    .into()
            },
            Instruction::Phi { source_info: _, incoming } => {
                let incoming: Vec<String> = incoming
                    .iter()
                    .map(|(block_id, inst_id)| format!("[{}: {}]", arena.get_block(*block_id).name, arena.inst_name(*inst_id)))
                    .collect();
                format!("{} {}", "phi".custom_color(CustomColor::new(202, 167, 244)), incoming.join(", ")).into()
            },
            Instruction::Tuple { source_info: _, instructions } => {
                let reprs: Vec<String> = instructions.iter().map(|id| arena.inst_name(*id)).collect();
                format!(
                    "{}{}{}",
                    "(".custom_color(CustomColor::new(129, 137, 150)),
                    reprs.join(", "),
                    ")".custom_color(CustomColor::new(129, 137, 150))
                )
                .into()
            },
            Instruction::Operation { source_info: _, op } => op.get_repr(arena),
            Instruction::Literal { source_info: _, literal } => match literal {
                Literal::Integer(lit) => lit.to_string().custom_color(CustomColor::new(250, 180, 134)),
                Literal::Float(lit) => lit.to_string().custom_color(CustomColor::new(250, 180, 134)),
                Literal::String(lit) => format!("\"{}\"", lit).custom_color(CustomColor::new(250, 180, 134)),
                Literal::Bool(lit) => lit.to_string().custom_color(CustomColor::new(250, 180, 134)),
            },
            Instruction::LoadElement { source_info: _, index, tuple } => format!(
                "{} {}, {}",
                "load_element".custom_color(CustomColor::new(202, 167, 244)),
                format!("{}", index).green(),
                arena.inst_name(*tuple)
            )
            .into(),
            Instruction::ReturnValue { source_info: _, type_id } => {
                format!("{} {}", "retval".custom_color(CustomColor::new(202, 167, 244)), arena.get_type(*type_id).get_repr(arena))
                    .into()
            },
            Instruction::Load { source_info: _, address, type_id: _ } => {
                format!("{} {}", "load".custom_color(CustomColor::new(202, 167, 244)), arena.inst_name(*address)).into()
            },
            Instruction::Store { source_info: _, address, value } => format!(
                "{} {}, {}",
                "store".custom_color(CustomColor::new(202, 167, 244)),
                arena.inst_name(*value),
                arena.inst_name(*address)
            )
            .into(),
            Instruction::Alloca { source_info: _, type_id } => format!(
                "{} {}",
                "alloca".custom_color(CustomColor::new(202, 167, 244)),
                format!("{}", arena.get_type(*type_id)).yellow()
            )
            .into(),
            Instruction::GlobalAlloca(_) => todo!("joao pls pls help ;_;"),
        }
    }
}

impl CfgPrintable for BasicBlock {
    fn get_repr(&self, arena: &IrArena) -> ColoredString {
        let non_term_inst_repr = self
            .instructions
            .iter()
            .map(|id| format!("    {} = {}", arena.inst_name(*id), arena.get_inst(*id).get_repr(arena)))
            .collect::<Vec<String>>()
            .join("\n");
        let terminator_repr = match self.terminator {
            Some(id) => format!("    {}", arena.get_term(id).get_repr(arena)),
            None => String::from("    <no terminator>"),
        };
        let mut parts = vec![format!("  {}:", self.name.red().bold())];
        if !non_term_inst_repr.is_empty() {
            parts.push(non_term_inst_repr);
        }
        parts.push(terminator_repr);
        parts.join("\n").into()
    }
}

fn strip_ansi(s: &str) -> String {
    let mut out = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            for c in chars.by_ref() {
                if c == 'm' {
                    break;
                }
            }
        } else {
            out.push(c);
        }
    }
    out
}

impl IrArena {
    pub fn generate_dot(&self, top_level: &Vec<IrTopLevelId>) -> String {
        let mut out = String::from("digraph CFG {\n");
        out.push_str("    node [shape=box fontname=\"Monospace\"]\n");

        for node in top_level {
            let IrTopLevelId::FunctionDecl(func_id) = node else {
                continue;
            };
            let func = self.get_func(*func_id);

            out.push_str(&format!("    subgraph cluster_{} {{\n", func.name));
            out.push_str(&format!("        label=\"fn {}\";\n", func.name));

            let blocks: Vec<_> = self.get_blocks_in(*func_id).iter().map(|block_id| self.get_block(*block_id)).collect();

            for block in blocks.clone() {
                let mut label_lines = vec![format!("{}:", block.name)];

                for inst_id in &block.instructions {
                    let inst = self.get_inst(*inst_id);
                    label_lines.push(format!("%{} = {}", inst_id, strip_ansi(&inst.get_repr(self).to_string())));
                }

                if let Some(term_id) = block.terminator {
                    label_lines.push(strip_ansi(&self.get_term(term_id).get_repr(self).to_string()));
                }

                let label = label_lines.join("\\l");
                let label = label.replace('"', "\\\"");
                out.push_str(&format!("        {}_{} [label=\"{}\\l\"];\n", func.name, block.name, label));
            }

            for block in blocks {
                let Some(term_id) = block.terminator else {
                    continue;
                };

                match self.get_term(term_id) {
                    TerminatorInst::Branch { true_block, false_block, .. } => {
                        let t = self.get_block(*true_block);
                        let f = self.get_block(*false_block);
                        out.push_str(&format!(
                            "        {}_{} -> {}_{} [label=\"true\"];\n",
                            func.name, block.name, func.name, t.name
                        ));
                        out.push_str(&format!(
                            "        {}_{} -> {}_{} [label=\"false\"];\n",
                            func.name, block.name, func.name, f.name
                        ));
                    },
                    TerminatorInst::Jump { source_info: _, target } => {
                        let t = self.get_block(*target);
                        out.push_str(&format!("        {}_{} -> {}_{};\n", func.name, block.name, func.name, t.name));
                    },
                    TerminatorInst::Return { .. } | TerminatorInst::Unreachable => {},
                }
            }

            out.push_str("    }\n");
        }

        out.push_str("}\n");
        out
    }
    pub fn dump(&self, top_level: &Vec<IrTopLevelId>) {
        eprintln!("total blocks: {}", self.blocks.len());
        for (i, block) in self.blocks.values().enumerate() {
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
                IrTopLevelId::GlobalVarDecl(id) => println!("{}", self.get_var(*id).get_repr(self)),
                IrTopLevelId::FunctionDecl(id) => println!("{}", self.get_func(*id).get_repr(self)),
            }
        }
        let dot = self.generate_dot(top_level);
        std::fs::write("cfg.dot", &dot).unwrap();
        eprintln!("\nWrote DOT file to cfg.dot. Run `dot -Tpng cfg.dot -o cfg.png` to convert into an image.");
    }
}
