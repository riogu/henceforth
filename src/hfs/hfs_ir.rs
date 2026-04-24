use std::{collections::HashSet, fmt::Display, vec};

use indexmap::IndexMap;
use slotmap::new_key_type;

use crate::hfs::{ast::*, ir_pretty_printing::prettify_ir, print, IrArena, Literal, SourceInfo};
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
    fn from(id: InstId) -> Self { InstOrTermId::InstId(id) }
}

impl From<TermInstId> for InstOrTermId {
    fn from(id: TermInstId) -> Self { InstOrTermId::TermInstId(id) }
}
impl Display for InstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.0.as_ffi() as u32) }
}
impl Display for TermInstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.0.as_ffi() as u32) }
}
impl Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.0.as_ffi() as u32) }
}
impl Display for IrFuncId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.0.as_ffi() as u32) }
}
impl Display for GlobalIrVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.0.as_ffi() as u32) }
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
impl IrArena {
    pub fn generate_dot(&self, top_level: &Vec<IrTopLevelId>) -> String {
        let mut out = String::from("digraph CFG {\n");
        out.push_str("    node [shape=box fontname=\"Monospace\"]\n");

        for node in top_level {
            let IrTopLevelId::FunctionDecl(func_id) = node else {
                continue;
            };

            let func_name = self.get_func(*func_id).name.clone();

            out.push_str(&format!("    subgraph cluster_{} {{\n", func_name));
            out.push_str(&format!("        label=\"fn {}\";\n", func_name));

            let mut names = crate::hfs::ir_syntax::NameMap::default();
            names.type_to_name.insert(crate::hfs::TypeId(0), "i32".to_string());
            names.type_to_name.insert(crate::hfs::TypeId(1), "f32".to_string());
            names.type_to_name.insert(crate::hfs::TypeId(2), "bool".to_string());
            names.type_to_name.insert(crate::hfs::TypeId(3), "str".to_string());

            let mut inst_counter = 0usize;
            for bid in self.get_blocks_in(*func_id) {
                for inst_id in &self.get_block(bid).instructions.clone() {
                    names.inst_to_name.insert(*inst_id, inst_counter);
                    names.name_to_inst.insert(inst_counter, *inst_id);
                    inst_counter += 1;
                }
            }
            for bid in self.get_blocks_in(*func_id) {
                let name = self.get_block(bid).name.clone();
                names.block_to_name.insert(bid, name.clone());
                names.unmangled_to_block.insert(name, bid);
            }
            for (fid, f) in self.functions.iter() {
                names.func_to_name.insert(fid, f.name.clone());
            }
            for (i, typ) in self.types.iter().enumerate() {
                let tid = crate::hfs::TypeId(i);
                if !names.type_to_name.contains_key(&tid) {
                    if let crate::hfs::Type::Tuple { type_ids, .. } = typ {
                        let inner: Vec<String> = type_ids
                            .iter()
                            .map(|id| names.type_to_name.get(id).cloned().unwrap_or_else(|| format!("t{}", id.0)))
                            .collect();
                        names.type_to_name.insert(tid, format!("({})", inner.join(" ")));
                    }
                }
            }

            let block_ids = self.get_blocks_in(*func_id);
            for block_id in &block_ids {
                let (block_name, inst_ids, terminator) = {
                    let block = self.get_block(*block_id);
                    (block.name.clone(), block.instructions.clone(), block.terminator)
                };

                let mut lines = vec![format!("{}:", block_name)];

                let mut type_ids = Vec::new();
                for inst_id in &inst_ids {
                    type_ids.push(self.get_type_id_of_inst_no_alloc(*inst_id).unwrap_or_default());
                }

                let printer = crate::hfs::ir_syntax::Printer;
                for ((inst_id, type_id), inst) in inst_ids.iter().zip(type_ids).map(|(id, tid)| {
                    let inst = self.get_inst(*id).clone();
                    ((*id, tid), inst)
                }) {
                    let inst_syntax = crate::hfs::ir_syntax::syntax_named_inst(&printer, &names);
                    let text =
                        (inst_syntax.0)((inst_id, type_id, inst)).unwrap_or_else(|| format!("<unprintable inst {:?}>", inst_id));
                    lines.push(format!("  {}", text));
                }

                if let Some(term_id) = terminator {
                    let term = self.get_term(term_id).clone();
                    let term_syntax = crate::hfs::ir_syntax::syntax_term(&printer, &names);
                    let text = (term_syntax.0)(term).unwrap_or_else(|| "<unprintable terminator>".to_string());
                    lines.push(format!("  {}", text));
                }

                let label = lines.join("\\l").replace('"', "\\\"");
                out.push_str(&format!("        {}_{} [label=\"{}\\l\"];\n", func_name, block_name, label));
            }

            for block_id in &block_ids {
                let (block_name, terminator) = {
                    let block = self.get_block(*block_id);
                    (block.name.clone(), block.terminator)
                };
                let Some(term_id) = terminator else {
                    continue;
                };
                match self.get_term(term_id).clone() {
                    TerminatorInst::Branch { true_block, false_block, .. } => {
                        let t_name = self.get_block(true_block).name.clone();
                        let f_name = self.get_block(false_block).name.clone();
                        out.push_str(&format!(
                            "        {}_{} -> {}_{} [label=\"true\"];\n",
                            func_name, block_name, func_name, t_name
                        ));
                        out.push_str(&format!(
                            "        {}_{} -> {}_{} [label=\"false\"];\n",
                            func_name, block_name, func_name, f_name
                        ));
                    },
                    TerminatorInst::Jump { target, .. } => {
                        let t_name = self.get_block(target).name.clone();
                        out.push_str(&format!("        {}_{} -> {}_{};\n", func_name, block_name, func_name, t_name));
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
        let repr = print(&self.functions.iter().map(|f| f.0).collect::<Vec<IrFuncId>>(), self);
        match repr {
            Some(repr) => println!("{}", prettify_ir(repr)),
            None => println!(""),
        }
        let dot = self.generate_dot(top_level);
        std::fs::write("cfg.dot", &dot).unwrap();
        eprintln!("\nWrote DOT file to cfg.dot. Run `dot -Tpng cfg.dot -o cfg.png` to convert into an image.");
    }
}
