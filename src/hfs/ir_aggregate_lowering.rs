use std::collections::HashMap;

use crate::hfs::{BlockId, InstId, Instruction, IrArena, IrFuncId, IrOperation, IrType, Literal, data_layout};

pub fn legalize_arrays(arena: &mut IrArena, func_id: IrFuncId) -> HashMap<InstId, u32> {
    let array_stores = collect_array_stores(arena, func_id);
    for block_id in arena.get_blocks_in(func_id) {
        for inst_id in arena.get_block(block_id).instructions.clone() {
            legalize_gep(arena, block_id, inst_id);
        }
    }
    array_stores
}

fn collect_array_stores(arena: &IrArena, func_id: IrFuncId) -> HashMap<InstId, u32> {
    let mut sizes = HashMap::new();
    for block_id in arena.get_blocks_in(func_id) {
        for &inst_id in &arena.get_block(block_id).instructions {
            let Instruction::Store { address, .. } = arena.get_inst(inst_id) else { continue };
            let dest_type = match arena.get_inst(*address) {
                Instruction::GetElementPtr { type_id, .. } | Instruction::Alloca { type_id, .. } => *type_id,
                _ => continue,
            };
            if matches!(arena.get_type(dest_type), IrType::Array { .. }) {
                sizes.insert(inst_id, data_layout::size_of(dest_type, arena));
            }
        }
    }
    sizes
}

fn legalize_gep(arena: &mut IrArena, block_id: BlockId, inst_id: InstId) {
    let Instruction::GetElementPtr { span, address, indexes, type_id } = arena.get_inst(inst_id).clone() else {
        return;
    };
    if indexes.len() != 1 {
        panic!("[internal error] array legalization expects exactly one GetElementPtr index, found {}", indexes.len());
    }
    let idx = indexes[0];
    let elem_size = data_layout::size_of(type_id, arena) as i32;

    let size_lit =
        arena.alloc_inst_before(Instruction::Literal { span: span.clone(), literal: Literal::Integer(elem_size) }, block_id, inst_id);
    let offset =
        arena.alloc_inst_before(Instruction::Operation { span: span.clone(), op: IrOperation::Mul(idx, size_lit) }, block_id, inst_id);
    *arena.get_inst_mut(inst_id) = Instruction::Operation { span, op: IrOperation::Add(offset, address) };
}
