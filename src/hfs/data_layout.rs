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

// lays out a sequence of possibly differently-typed values one after another, each at its own
// natural alignment - like a plain, unpacked struct. Used for a function's StructReturn buffer,
// where several logically separate return values are bundled into one caller-provided block of
// memory. Returns each value's offset, the buffer's total size, and its required alignment.
pub fn sequential_layout(type_ids: &[TypeId], arena: &IrArena) -> (Vec<u32>, u32, u32) {
    let mut offsets = Vec::with_capacity(type_ids.len());
    let mut cursor: u32 = 0;
    let mut max_align: u32 = 1;
    for &type_id in type_ids {
        let align = align_of(type_id, arena);
        max_align = max_align.max(align);
        cursor = cursor.div_ceil(align) * align;
        offsets.push(cursor);
        cursor += size_of(type_id, arena);
    }
    (offsets, cursor, max_align)
}
