use crate::hfs::{BlockId, InstId, Instruction, IrArena, IrFuncId, IrOperation, Literal, data_layout};

pub fn legalize_arrays(arena: &mut IrArena, func_id: IrFuncId) {
    for block_id in arena.get_blocks_in(func_id) {
        for inst_id in arena.get_block(block_id).instructions.clone() {
            legalize_gep(arena, block_id, inst_id);
        }
    }
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
