use std::collections::HashMap;

use cranelift_codegen::ir::{self, InstBuilder, MemFlags, StackSlotData, StackSlotKind, condcodes::IntCC};
use cranelift_frontend::FunctionBuilder;
use cranelift_module::{DataDescription, DataId, FuncId as ClifFuncId, Linkage, Module};

use crate::hfs::{Builtin, InstId, IrArena, cranelift_translate::infer_clif_type};

pub struct BuiltinsContext {
    printf: ClifFuncId,
    scanf: ClifFuncId,
    getline: ClifFuncId,
    memcmp: ClifFuncId,
    stdin: DataId,
    fmt_d: DataId,
    fmt_f: DataId,
    fmt_g: DataId,
    fmt_s: DataId,
    fmt_str: DataId,
    str_true: DataId,
    str_false: DataId,
    empty: DataId,
}

pub fn declare_builtins(module: &mut dyn Module) -> BuiltinsContext {
    let ptr_ty = module.target_config().pointer_type();
    BuiltinsContext {
        printf: declare_libc_fn(module, "printf", &[ptr_ty], &[ir::types::I32]),
        scanf: declare_libc_fn(module, "scanf", &[ptr_ty], &[ir::types::I32]),
        getline: declare_libc_fn(module, "getline", &[ptr_ty, ptr_ty, ptr_ty], &[ir::types::I64]),
        memcmp: declare_libc_fn(module, "memcmp", &[ptr_ty, ptr_ty, ir::types::I64], &[ir::types::I32]),
        stdin: module
            .declare_data("stdin", Linkage::Import, false, false)
            .unwrap_or_else(|e| panic!("failed to declare 'stdin': {e}")),
        fmt_d: declare_cstring(module, "__hfs_fmt_d", "%d"),
        fmt_f: declare_cstring(module, "__hfs_fmt_f", "%f"),
        fmt_g: declare_cstring(module, "__hfs_fmt_g", "%g"),
        fmt_s: declare_cstring(module, "__hfs_fmt_s", "%s"),
        fmt_str: declare_cstring(module, "__hfs_fmt_str", "%.*s"),
        str_true: declare_cstring(module, "__hfs_str_true", "true"),
        str_false: declare_cstring(module, "__hfs_str_false", "false"),
        empty: declare_cstring(module, "__hfs_empty_str", ""),
    }
}

fn declare_libc_fn(module: &mut dyn Module, name: &str, params: &[ir::Type], returns: &[ir::Type]) -> ClifFuncId {
    let mut sig = module.make_signature();
    for &param in params {
        sig.params.push(ir::AbiParam::new(param));
    }
    for &ret in returns {
        sig.returns.push(ir::AbiParam::new(ret));
    }
    module
        .declare_function(name, Linkage::Import, &sig)
        .unwrap_or_else(|e| panic!("[cranelift backend] failed to declare '{name}': {e}"))
}

pub fn declare_cstring(module: &mut dyn Module, name: &str, s: &str) -> DataId {
    let data_id = module
        .declare_data(name, Linkage::Local, false, false)
        .unwrap_or_else(|e| panic!("[cranelift backend] failed to declare '{name}': {e}"));
    let mut bytes = s.as_bytes().to_vec();
    bytes.push(0);
    let mut desc = DataDescription::new();
    desc.define(bytes.into_boxed_slice());
    module.define_data(data_id, &desc).unwrap_or_else(|e| panic!("[cranelift backend] failed to define '{name}': {e}"));
    data_id
}

pub fn data_ptr(builder: &mut FunctionBuilder, module: &mut dyn Module, data: DataId, ptr_ty: ir::Type) -> ir::Value {
    let gv = module.declare_data_in_func(data, builder.func);
    builder.ins().global_value(ptr_ty, gv)
}

fn call_variadic(
    builder: &mut FunctionBuilder,
    module: &mut dyn Module,
    func_id: ClifFuncId,
    params: &[ir::Type],
    args: &[ir::Value],
) {
    let func_ref = module.declare_func_in_func(func_id, builder.func);
    let mut sig = module.make_signature();
    for &param in params {
        sig.params.push(ir::AbiParam::new(param));
    }
    sig.returns.push(ir::AbiParam::new(ir::types::I32));
    let sig_ref = builder.import_signature(sig);
    let ptr_ty = module.target_config().pointer_type();
    let addr = builder.ins().func_addr(ptr_ty, func_ref);
    builder.ins().call_indirect(sig_ref, addr, args);
}

pub fn string_eq(builder: &mut FunctionBuilder, module: &mut dyn Module, ctx: &BuiltinsContext, a: ir::Value, b: ir::Value) -> ir::Value {
    let (ptr_a, len_a) = builder.ins().isplit(a);
    let (ptr_b, len_b) = builder.ins().isplit(b);
    let same_len = builder.ins().icmp(IntCC::Equal, len_a, len_b);
    let a_shorter = builder.ins().icmp(IntCC::UnsignedLessThan, len_a, len_b);
    let compare_len = builder.ins().select(a_shorter, len_a, len_b);
    let func_ref = module.declare_func_in_func(ctx.memcmp, builder.func);
    let call_inst = builder.ins().call(func_ref, &[ptr_a, ptr_b, compare_len]);
    let cmp_result = builder.inst_results(call_inst)[0];
    let same_bytes = builder.ins().icmp_imm(IntCC::Equal, cmp_result, 0);
    builder.ins().band(same_len, same_bytes)
}

pub fn translate_builtin_call(
    builtin: Builtin,
    args: &[InstId],
    return_values: &[InstId],
    arena: &IrArena,
    builder: &mut FunctionBuilder,
    module: &mut dyn Module,
    ctx: &BuiltinsContext,
    values: &mut HashMap<InstId, ir::Value>,
) {
    let ptr_ty = module.target_config().pointer_type();
    match builtin {
        Builtin::Print => translate_print(args[0], arena, builder, module, ctx, values, ptr_ty),
        Builtin::InputInt => translate_input_int(return_values[0], builder, module, ctx, values, ptr_ty),
        Builtin::InputFloat => translate_input_float(return_values[0], builder, module, ctx, values, ptr_ty),
        Builtin::InputStr => translate_input_str(return_values[0], builder, module, ctx, values, ptr_ty),
    }
}

fn translate_print(
    arg: InstId,
    arena: &IrArena,
    builder: &mut FunctionBuilder,
    module: &mut dyn Module,
    ctx: &BuiltinsContext,
    values: &HashMap<InstId, ir::Value>,
    ptr_ty: ir::Type,
) {
    let val = values[&arg];
    match infer_clif_type(arg, arena) {
        ir::types::I32 => {
            let fmt = data_ptr(builder, module, ctx.fmt_d, ptr_ty);
            call_variadic(builder, module, ctx.printf, &[ptr_ty, ir::types::I32], &[fmt, val]);
        },
        ir::types::F32 => {
            let fmt = data_ptr(builder, module, ctx.fmt_g, ptr_ty);
            let widened = builder.ins().fpromote(ir::types::F64, val);
            call_variadic(builder, module, ctx.printf, &[ptr_ty, ir::types::F64], &[fmt, widened]);
        },
        ir::types::I8 => {
            let fmt = data_ptr(builder, module, ctx.fmt_s, ptr_ty);
            let true_ptr = data_ptr(builder, module, ctx.str_true, ptr_ty);
            let false_ptr = data_ptr(builder, module, ctx.str_false, ptr_ty);
            let chosen = builder.ins().select(val, true_ptr, false_ptr);
            call_variadic(builder, module, ctx.printf, &[ptr_ty, ptr_ty], &[fmt, chosen]);
        },
        ir::types::I128 => {
            let (str_ptr, len) = builder.ins().isplit(val);
            let len32 = builder.ins().ireduce(ir::types::I32, len);
            let fmt = data_ptr(builder, module, ctx.fmt_str, ptr_ty);
            call_variadic(builder, module, ctx.printf, &[ptr_ty, ir::types::I32, ptr_ty], &[fmt, len32, str_ptr]);
        },
        ir::types::I64 => panic!("[cranelift backend] print doesn't support arrays yet"),
        other => panic!("[cranelift backend] print doesn't support values of Cranelift type {other}"),
    }
}

fn translate_input_int(
    return_value: InstId,
    builder: &mut FunctionBuilder,
    module: &mut dyn Module,
    ctx: &BuiltinsContext,
    values: &mut HashMap<InstId, ir::Value>,
    ptr_ty: ir::Type,
) {
    let slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 4, 2));
    let addr = builder.ins().stack_addr(ptr_ty, slot, 0);
    let zero = builder.ins().iconst(ir::types::I32, 0);
    builder.ins().store(MemFlags::trusted(), zero, addr, 0);
    let fmt = data_ptr(builder, module, ctx.fmt_d, ptr_ty);
    call_variadic(builder, module, ctx.scanf, &[ptr_ty, ptr_ty], &[fmt, addr]);
    let val = builder.ins().load(ir::types::I32, MemFlags::trusted(), addr, 0);
    values.insert(return_value, val);
}

fn translate_input_float(
    return_value: InstId,
    builder: &mut FunctionBuilder,
    module: &mut dyn Module,
    ctx: &BuiltinsContext,
    values: &mut HashMap<InstId, ir::Value>,
    ptr_ty: ir::Type,
) {
    let slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 4, 2));
    let addr = builder.ins().stack_addr(ptr_ty, slot, 0);
    let zero = builder.ins().f32const(0.0);
    builder.ins().store(MemFlags::trusted(), zero, addr, 0);
    let fmt = data_ptr(builder, module, ctx.fmt_f, ptr_ty);
    call_variadic(builder, module, ctx.scanf, &[ptr_ty, ptr_ty], &[fmt, addr]);
    let val = builder.ins().load(ir::types::F32, MemFlags::trusted(), addr, 0);
    values.insert(return_value, val);
}

fn translate_input_str(
    return_value: InstId,
    builder: &mut FunctionBuilder,
    module: &mut dyn Module,
    ctx: &BuiltinsContext,
    values: &mut HashMap<InstId, ir::Value>,
    ptr_ty: ir::Type,
) {
    let line_ptr_slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 3));
    let line_cap_slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 3));
    let line_ptr_addr = builder.ins().stack_addr(ptr_ty, line_ptr_slot, 0);
    let line_cap_addr = builder.ins().stack_addr(ptr_ty, line_cap_slot, 0);
    let zero_ptr = builder.ins().iconst(ptr_ty, 0);
    let zero_cap = builder.ins().iconst(ir::types::I64, 0);
    builder.ins().store(MemFlags::trusted(), zero_ptr, line_ptr_addr, 0);
    builder.ins().store(MemFlags::trusted(), zero_cap, line_cap_addr, 0);

    let stdin_gv = module.declare_data_in_func(ctx.stdin, builder.func);
    let stdin_addr = builder.ins().global_value(ptr_ty, stdin_gv);
    let stdin_file = builder.ins().load(ptr_ty, MemFlags::trusted(), stdin_addr, 0);

    let getline_ref = module.declare_func_in_func(ctx.getline, builder.func);
    let call_inst = builder.ins().call(getline_ref, &[line_ptr_addr, line_cap_addr, stdin_file]);
    let raw_result = builder.inst_results(call_inst)[0];

    let zero64 = builder.ins().iconst(ir::types::I64, 0);
    let one64 = builder.ins().iconst(ir::types::I64, 1);
    let read_failed = builder.ins().icmp(IntCC::SignedLessThan, raw_result, zero64);
    let len0 = builder.ins().select(read_failed, zero64, raw_result);

    let real_ptr = builder.ins().load(ptr_ty, MemFlags::trusted(), line_ptr_addr, 0);
    let safe_ptr = data_ptr(builder, module, ctx.empty, ptr_ty);
    let str_ptr = builder.ins().select(read_failed, safe_ptr, real_ptr);

    let lf = builder.ins().iconst(ir::types::I8, b'\n' as i64);
    let cr = builder.ins().iconst(ir::types::I8, b'\r' as i64);

    let len_gt_0_a = builder.ins().icmp(IntCC::SignedGreaterThan, len0, zero64);
    let len_minus_1_a = builder.ins().isub(len0, one64);
    let idx_a = builder.ins().select(len_gt_0_a, len_minus_1_a, zero64);
    let addr_a = builder.ins().iadd(str_ptr, idx_a);
    let byte_a = builder.ins().load(ir::types::I8, MemFlags::trusted(), addr_a, 0);
    let is_lf = builder.ins().icmp(IntCC::Equal, byte_a, lf);
    let strip_lf = builder.ins().band(len_gt_0_a, is_lf);
    let len1 = builder.ins().select(strip_lf, len_minus_1_a, len0);

    let len_gt_0_b = builder.ins().icmp(IntCC::SignedGreaterThan, len1, zero64);
    let len_minus_1_b = builder.ins().isub(len1, one64);
    let idx_b = builder.ins().select(len_gt_0_b, len_minus_1_b, zero64);
    let addr_b = builder.ins().iadd(str_ptr, idx_b);
    let byte_b = builder.ins().load(ir::types::I8, MemFlags::trusted(), addr_b, 0);
    let is_cr = builder.ins().icmp(IntCC::Equal, byte_b, cr);
    let strip_cr = builder.ins().band(len_gt_0_b, is_cr);
    let len2 = builder.ins().select(strip_cr, len_minus_1_b, len1);

    let string_val = builder.ins().iconcat(str_ptr, len2);
    values.insert(return_value, string_val);
}
