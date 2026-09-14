use crate::hfs::{InstId, Instruction, IrArena, IrType, Literal, Type, TypeId};

// target pointer width; there's only one target (host, 64-bit) for now
pub const POINTER_SIZE: u32 = 8;

// no mixed-field structs exist, so this is just primitive sizes and index * element
// size for arrays; nothing here needs padding/alignment beyond natural size
pub fn size_of(type_id: TypeId, arena: &IrArena) -> u32 {
    let ty = arena.get_type(type_id);
    if ty.get_ptr_count() > 0 {
        return POINTER_SIZE;
    }
    match ty {
        IrType::Int { .. } => 4,
        IrType::Float { .. } => 4,
        IrType::Bool { .. } => 1,
        // {ptr: i64, len: i64}
        IrType::String { .. } => 16,
        IrType::Tuple { .. } => panic!("[internal error] tuples are never laid out in memory, size_of doesn't apply"),
        IrType::Array { hfs_type, length, .. } => {
            let len = const_array_len(*length, arena)
                .expect("[internal error] size_of called on an array whose length isn't a compile-time constant");
            size_of(*hfs_type, arena) * len
        },
    }
}

pub fn align_of(type_id: TypeId, arena: &IrArena) -> u32 {
    match arena.get_type(type_id) {
        IrType::Array { hfs_type, .. } => align_of(*hfs_type, arena),
        _ => size_of(type_id, arena),
    }
}

// mirrors how RuntimeValue::default (interpreter.rs) reads a constant array length
pub fn const_array_len(length: Option<InstId>, arena: &IrArena) -> Option<u32> {
    match length.map(|inst| arena.get_inst(inst)) {
        Some(Instruction::Literal { literal: Literal::Integer(n), .. }) => Some(*n as u32),
        _ => None,
    }
}
