use std::collections::HashMap;

use cranelift_codegen::ir::{self, InstBuilder, MemFlags, StackSlotData, StackSlotKind};
use cranelift_frontend::FunctionBuilder;
use cranelift_module::{DataDescription, DataId, FuncId as ClifFuncId, Linkage, Module};

use crate::hfs::{Builtin, InstId, IrArena, cranelift_translate::infer_clif_type};

// printf/scanf are C-variadic, but a Cranelift call site needs a fixed signature matching the
// actual arguments passed - and cranelift-module rejects declaring the same symbol twice under
// different signatures (Module::declare_function merges into the existing one and errors on a
// mismatch). So each of these is declared once, under a minimal fixed-arity signature just to
// get a FuncRef/address; every real call site then builds its own signature and calls through
// that address indirectly (see call_variadic) instead of calling the declared FuncRef directly.
pub struct BuiltinsContext {
    printf: ClifFuncId,
    scanf: ClifFuncId,
    fmt_d: DataId,
    fmt_f: DataId,
    fmt_g: DataId,
    fmt_s: DataId,
    str_true: DataId,
    str_false: DataId,
}

pub fn declare_builtins(module: &mut dyn Module) -> BuiltinsContext {
    let ptr_ty = module.target_config().pointer_type();
    BuiltinsContext {
        printf: declare_libc_fn(module, "printf", &[ptr_ty], &[ir::types::I32]),
        scanf: declare_libc_fn(module, "scanf", &[ptr_ty], &[ir::types::I32]),
        fmt_d: declare_cstring(module, "__hfs_fmt_d", "%d"),
        fmt_f: declare_cstring(module, "__hfs_fmt_f", "%f"),
        fmt_g: declare_cstring(module, "__hfs_fmt_g", "%g"),
        fmt_s: declare_cstring(module, "__hfs_fmt_s", "%s"),
        str_true: declare_cstring(module, "__hfs_str_true", "true"),
        str_false: declare_cstring(module, "__hfs_str_false", "false"),
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

fn declare_cstring(module: &mut dyn Module, name: &str, s: &str) -> DataId {
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

fn data_ptr(builder: &mut FunctionBuilder, module: &mut dyn Module, data: DataId, ptr_ty: ir::Type) -> ir::Value {
    let gv = module.declare_data_in_func(data, builder.func);
    builder.ins().global_value(ptr_ty, gv)
}

// imports func_id's address under a signature built just for this call site, then calls through
// it indirectly - the only way to give one linked symbol (e.g. "printf") different argument
// shapes at different call sites, since a direct call is pinned to the signature it was
// declared with at the Module level.
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
        Builtin::InputStr => panic!("[cranelift backend] input_str isn't supported yet (see Phase 4)"),
    }
}

// print's declared parameter type (builtins.rs) is a placeholder - the real type of the value
// being printed is whatever infer_clif_type finds by tracing the argument back to its origin,
// same as arithmetic instructions already do to pick an int vs. float opcode.
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
            // printf reads a variadic float argument as a double - the caller (not the ABI)
            // is responsible for widening it first, same as C itself does at the call site.
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
        other => panic!("[cranelift backend] print doesn't support values of Cranelift type {other}"),
    }
}

// matches the interpreter's own fallback: malformed/EOF input becomes 0/0.0 rather than an
// error, so the slot scanf writes into starts zeroed instead of holding whatever garbage a
// failed scanf call would leave behind.
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
