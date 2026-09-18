use std::collections::HashMap;

use cranelift_codegen::ir::{
    self, InstBuilder, MemFlags, StackSlotData, StackSlotKind,
    condcodes::{FloatCC, IntCC},
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{FuncId as ClifFuncId, Linkage, Module};

use crate::hfs::{
    BlockId, InstId, Instruction, IrArena, IrFuncId, IrFunction, IrOperation, IrType, Literal, TerminatorInst, Type, TypeId,
    cranelift_builtins::{BuiltinsContext, data_ptr, declare_cstring, translate_builtin_call},
    data_layout, find_builtin,
};

pub fn ir_type_to_clif(type_id: TypeId, arena: &IrArena) -> ir::Type {
    let ty = arena.get_type(type_id);
    if ty.get_ptr_count() > 0 {
        return ir::types::I64;
    }
    match ty {
        IrType::Int { .. } => ir::types::I32,
        IrType::Float { .. } => ir::types::F32,
        IrType::Bool { .. } => ir::types::I8,
        IrType::String { .. } => ir::types::I128,
        IrType::Array { .. } => ir::types::I64,
        IrType::Tuple { .. } => panic!("[cranelift backend] a Tuple type has no single Cranelift representation"),
    }
}

fn return_type_ids(func: &IrFunction, arena: &IrArena) -> Vec<TypeId> {
    let IrType::Tuple { type_ids, .. } = arena.get_type(func.return_type) else {
        panic!("[internal error] a function's return_type is always a Tuple, even for 0 or 1 values")
    };
    type_ids.clone()
}

fn uses_struct_return(return_types: &[TypeId]) -> bool { return_types.len() > 1 }

fn is_main(func: &IrFunction) -> bool { func.name == "main" }

fn make_signature(func: &IrFunction, arena: &IrArena, module: &dyn Module) -> ir::Signature {
    let mut sig = module.make_signature();
    let return_types = return_type_ids(func, arena);
    if !is_main(func) && uses_struct_return(&return_types) {
        sig.params.push(ir::AbiParam::special(module.target_config().pointer_type(), ir::ArgumentPurpose::StructReturn));
    }
    let IrType::Tuple { type_ids: param_types, .. } = arena.get_type(func.param_type) else {
        panic!("[internal error] a function's param_type is always a Tuple, even for 0 or 1 values")
    };
    for type_id in param_types {
        sig.params.push(ir::AbiParam::new(ir_type_to_clif(*type_id, arena)));
    }
    if is_main(func) {
        sig.returns.push(ir::AbiParam::new(ir::types::I32));
    } else if !uses_struct_return(&return_types) {
        for type_id in &return_types {
            sig.returns.push(ir::AbiParam::new(ir_type_to_clif(*type_id, arena)));
        }
    }
    sig
}

pub fn declare_all_functions(arena: &IrArena, module: &mut dyn Module) -> HashMap<IrFuncId, ClifFuncId> {
    let mut func_ids = HashMap::new();
    for (func_id, func) in arena.functions.iter() {
        if find_builtin(&func.name).is_some() {
            continue;
        }
        let sig = make_signature(func, arena, module);
        let linkage = if func.name == "main" { Linkage::Export } else { Linkage::Local };
        let clif_id = module
            .declare_function(&func.name, linkage, &sig)
            .unwrap_or_else(|e| panic!("[cranelift backend] failed to declare '{}': {e}", func.name));
        func_ids.insert(func_id, clif_id);
    }
    func_ids
}

pub fn infer_clif_type(inst_id: InstId, arena: &IrArena) -> ir::Type {
    try_infer_clif_type(inst_id, arena, &mut Vec::new()).unwrap_or_else(|| {
        panic!(
            "[cranelift backend] couldn't find a terminal type for this value - every path leads back through a phi with \
             no other edge to resolve. This happens when a `while` loop carries a value across iterations on the stack \
             instead of through a named variable: unlike if/else, while loops don't build a phi for that (see \
             generate_merge_phis in ir_lowerer.rs), so the value can end up depending on itself with nothing to break the \
             cycle."
        )
    })
}

fn try_infer_clif_type(inst_id: InstId, arena: &IrArena, visiting: &mut Vec<InstId>) -> Option<ir::Type> {
    if visiting.contains(&inst_id) {
        return None;
    }
    match arena.get_inst(inst_id) {
        Instruction::Literal { literal, .. } => Some(match literal {
            Literal::Integer(_) => ir::types::I32,
            Literal::Float(_) => ir::types::F32,
            Literal::Bool(_) => ir::types::I8,
            Literal::String(_) => ir::types::I128,
        }),
        Instruction::Load { type_id, .. }
        | Instruction::Alloca { type_id, .. }
        | Instruction::Parameter { type_id, .. }
        | Instruction::ReturnValue { type_id, .. }
        | Instruction::GetElementPtr { type_id, .. } => Some(ir_type_to_clif(*type_id, arena)),
        Instruction::Phi { incoming, .. } => {
            visiting.push(inst_id);
            let mut result = None;
            for &candidate in incoming.values() {
                result = try_infer_clif_type(candidate, arena, visiting);
                if result.is_some() {
                    break;
                }
            }
            visiting.pop();
            result
        },
        Instruction::Operation { op, .. } => match op {
            IrOperation::Equal(..)
            | IrOperation::NotEqual(..)
            | IrOperation::Less(..)
            | IrOperation::LessEqual(..)
            | IrOperation::Greater(..)
            | IrOperation::GreaterEqual(..)
            | IrOperation::Or(..)
            | IrOperation::And(..)
            | IrOperation::Not(..) => Some(ir::types::I8),
            IrOperation::Add(l, ..) | IrOperation::Sub(l, ..) | IrOperation::Mul(l, ..) | IrOperation::Div(l, ..) | IrOperation::Mod(l, ..) =>
                try_infer_clif_type(*l, arena, visiting),
        },
        other => panic!("[cranelift backend] can't infer a scalar Cranelift type for {other:?}"),
    }
}

fn is_float(inst_id: InstId, arena: &IrArena) -> bool { infer_clif_type(inst_id, arena) == ir::types::F32 }

fn match_int_widths(builder: &mut FunctionBuilder, a: ir::Value, b: ir::Value) -> (ir::Value, ir::Value) {
    let ty_a = builder.func.dfg.value_type(a);
    let ty_b = builder.func.dfg.value_type(b);
    if ty_a == ty_b {
        (a, b)
    } else if ty_a.bits() < ty_b.bits() {
        (builder.ins().sextend(ty_b, a), b)
    } else {
        (a, builder.ins().sextend(ty_a, b))
    }
}

fn translate_operation(op: IrOperation, arena: &IrArena, builder: &mut FunctionBuilder, values: &HashMap<InstId, ir::Value>) -> ir::Value {
    let get = |id: InstId| values[&id];
    match op {
        IrOperation::Add(l, r) =>
            if is_float(l, arena) {
                builder.ins().fadd(get(l), get(r))
            } else {
                let (lv, rv) = match_int_widths(builder, get(l), get(r));
                builder.ins().iadd(lv, rv)
            },
        IrOperation::Sub(l, r) =>
            if is_float(l, arena) {
                builder.ins().fsub(get(l), get(r))
            } else {
                let (lv, rv) = match_int_widths(builder, get(l), get(r));
                builder.ins().isub(lv, rv)
            },
        IrOperation::Mul(l, r) =>
            if is_float(l, arena) {
                builder.ins().fmul(get(l), get(r))
            } else {
                let (lv, rv) = match_int_widths(builder, get(l), get(r));
                builder.ins().imul(lv, rv)
            },
        IrOperation::Div(l, r) =>
            if is_float(l, arena) {
                builder.ins().fdiv(get(l), get(r))
            } else {
                builder.ins().sdiv(get(l), get(r))
            },
        IrOperation::Mod(l, r) =>
            if is_float(l, arena) {
                panic!("[cranelift backend] float modulo isn't supported yet")
            } else {
                builder.ins().srem(get(l), get(r))
            },
        IrOperation::Equal(l, r) =>
            if is_float(l, arena) {
                builder.ins().fcmp(FloatCC::Equal, get(l), get(r))
            } else {
                builder.ins().icmp(IntCC::Equal, get(l), get(r))
            },
        IrOperation::NotEqual(l, r) =>
            if is_float(l, arena) {
                builder.ins().fcmp(FloatCC::NotEqual, get(l), get(r))
            } else {
                builder.ins().icmp(IntCC::NotEqual, get(l), get(r))
            },
        IrOperation::Less(l, r) =>
            if is_float(l, arena) {
                builder.ins().fcmp(FloatCC::LessThan, get(l), get(r))
            } else {
                builder.ins().icmp(IntCC::SignedLessThan, get(l), get(r))
            },
        IrOperation::LessEqual(l, r) =>
            if is_float(l, arena) {
                builder.ins().fcmp(FloatCC::LessThanOrEqual, get(l), get(r))
            } else {
                builder.ins().icmp(IntCC::SignedLessThanOrEqual, get(l), get(r))
            },
        IrOperation::Greater(l, r) =>
            if is_float(l, arena) {
                builder.ins().fcmp(FloatCC::GreaterThan, get(l), get(r))
            } else {
                builder.ins().icmp(IntCC::SignedGreaterThan, get(l), get(r))
            },
        IrOperation::GreaterEqual(l, r) =>
            if is_float(l, arena) {
                builder.ins().fcmp(FloatCC::GreaterThanOrEqual, get(l), get(r))
            } else {
                builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, get(l), get(r))
            },
        IrOperation::Or(l, r) => builder.ins().bor(get(l), get(r)),
        IrOperation::And(l, r) => builder.ins().band(get(l), get(r)),
        IrOperation::Not(x) => builder.ins().bxor_imm(get(x), 1),
    }
}

fn translate_instruction(
    inst_id: InstId,
    arena: &IrArena,
    builder: &mut FunctionBuilder,
    module: &mut dyn Module,
    func_ids: &HashMap<IrFuncId, ClifFuncId>,
    builtins_ctx: &BuiltinsContext,
    values: &mut HashMap<InstId, ir::Value>,
) {
    match arena.get_inst(inst_id).clone() {
        Instruction::Phi { .. } | Instruction::Parameter { .. } | Instruction::ReturnValue { .. } | Instruction::Tuple { .. } => {},
        Instruction::Literal { literal, .. } => {
            let val = match literal {
                Literal::Integer(n) => builder.ins().iconst(ir::types::I32, n as i64),
                Literal::Float(f) => builder.ins().f32const(f),
                Literal::Bool(b) => builder.ins().iconst(ir::types::I8, b as i64),
                Literal::String(s) => {
                    let name = format!("__hfs_str_lit_{:x}", slotmap::Key::data(&inst_id).as_ffi());
                    let data_id = declare_cstring(module, &name, &s);
                    let ptr_ty = module.target_config().pointer_type();
                    let ptr = data_ptr(builder, module, data_id, ptr_ty);
                    let len = builder.ins().iconst(ir::types::I64, s.len() as i64);
                    builder.ins().iconcat(ptr, len)
                },
            };
            values.insert(inst_id, val);
        },
        Instruction::Operation { op, .. } => {
            let val = translate_operation(op, arena, builder, values);
            values.insert(inst_id, val);
        },
        Instruction::Alloca { type_id, array_len, .. } => {
            let size = match arena.get_type(type_id) {
                IrType::Array { hfs_type: elem_type, .. } => {
                    let Some(len) = data_layout::try_const_len(array_len, arena) else {
                        panic!(
                            "[cranelift backend] array has a non-constant length and was never bound with &= - only \
                             &= (aliasing, no allocation) supports a runtime-sized array; using one directly, or \
                             binding it with :=, needs a real, compile-time-sized array"
                        )
                    };
                    data_layout::size_of(*elem_type, arena) * len
                },
                _ => data_layout::size_of(type_id, arena),
            };
            let align_shift = data_layout::align_of(type_id, arena).trailing_zeros() as u8;
            let slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, size, align_shift));
            let addr = builder.ins().stack_addr(module.target_config().pointer_type(), slot, 0);
            values.insert(inst_id, addr);
        },
        Instruction::Load { address, type_id, .. } => {
            if matches!(arena.get_type(type_id), IrType::Array { .. }) {
                let addr = values[&address];
                values.insert(inst_id, addr);
                return;
            }
            let val = builder.ins().load(ir_type_to_clif(type_id, arena), MemFlags::trusted(), values[&address], 0);
            values.insert(inst_id, val);
        },
        Instruction::Store { address, value, .. } => {
            builder.ins().store(MemFlags::trusted(), values[&value], values[&address], 0);
        },
        Instruction::FunctionCall { args, func_id, return_values, .. } => {
            let callee = arena.get_func(func_id);
            if let Some(builtin) = find_builtin(&callee.name) {
                translate_builtin_call(builtin.builtin, &args, &return_values, arena, builder, module, builtins_ctx, values);
                return;
            }
            let func_ref = module.declare_func_in_func(func_ids[&func_id], builder.func);
            let callee_returns = return_type_ids(callee, arena);

            let mut arg_vals = Vec::new();
            let sret_addr = if uses_struct_return(&callee_returns) {
                let (_, size, align) = data_layout::sequential_layout(&callee_returns, arena);
                let slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, size, align.trailing_zeros() as u8));
                let addr = builder.ins().stack_addr(module.target_config().pointer_type(), slot, 0);
                arg_vals.push(addr);
                Some(addr)
            } else {
                None
            };
            for a in &args {
                arg_vals.push(values[a]);
            }
            let call_inst = builder.ins().call(func_ref, &arg_vals);

            match sret_addr {
                Some(addr) => {
                    let (offsets, ..) = data_layout::sequential_layout(&callee_returns, arena);
                    for (i, &retval_inst) in return_values.iter().enumerate() {
                        let ty = ir_type_to_clif(callee_returns[i], arena);
                        let val = builder.ins().load(ty, MemFlags::trusted(), addr, offsets[i] as i32);
                        values.insert(retval_inst, val);
                    }
                },
                None => {
                    let results = builder.inst_results(call_inst).to_vec();
                    for (retval_inst, result) in return_values.iter().zip(results) {
                        values.insert(*retval_inst, result);
                    }
                },
            }
        },
        Instruction::GlobalAlloca(_) => panic!("[cranelift backend] global variables aren't supported yet"),
        Instruction::GetElementPtr { .. } => panic!("[internal error] a GetElementPtr survived array legalization"),
        Instruction::LoadElement { .. } => panic!("[cranelift backend] tuple element access isn't supported"),
    }
}

fn block_args_for(
    target: BlockId,
    from: BlockId,
    arena: &IrArena,
    phi_order: &HashMap<BlockId, Vec<InstId>>,
    values: &HashMap<InstId, ir::Value>,
) -> Vec<ir::BlockArg> {
    let mut args = Vec::new();
    for phi_inst in &phi_order[&target] {
        let Instruction::Phi { incoming, .. } = arena.get_inst(*phi_inst) else {
            panic!("[internal error] phi_order must only contain Instruction::Phi")
        };
        let incoming_val = incoming
            .get(&from)
            .unwrap_or_else(|| panic!("[internal error] jump to a block whose phi has no entry for predecessor {from:?}"));
        args.push(ir::BlockArg::from(values[incoming_val]));
    }
    args
}

fn translate_terminator(
    term_id: crate::hfs::TermInstId,
    block_id: BlockId,
    arena: &IrArena,
    builder: &mut FunctionBuilder,
    clif_blocks: &HashMap<BlockId, ir::Block>,
    phi_order: &HashMap<BlockId, Vec<InstId>>,
    values: &HashMap<InstId, ir::Value>,
    return_types: &[TypeId],
    sret_ptr: Option<ir::Value>,
    translating_main: bool,
) {
    match arena.get_term(term_id) {
        TerminatorInst::Jump { target, .. } => {
            let args = block_args_for(*target, block_id, arena, phi_order, values);
            builder.ins().jump(clif_blocks[target], &args);
        },
        TerminatorInst::Branch { cond, true_block, false_block, .. } => {
            let true_args = block_args_for(*true_block, block_id, arena, phi_order, values);
            let false_args = block_args_for(*false_block, block_id, arena, phi_order, values);
            builder.ins().brif(values[cond], clif_blocks[true_block], &true_args, clif_blocks[false_block], &false_args);
        },
        TerminatorInst::Return { return_tuple, .. } => {
            let Instruction::Tuple { instructions, .. } = arena.get_inst(*return_tuple) else {
                panic!("[internal error] a Return's return_tuple is always an Instruction::Tuple")
            };
            if translating_main {
                let exit_value = match (instructions.as_slice(), return_types) {
                    ([single], [return_type]) if ir_type_to_clif(*return_type, arena) == ir::types::I32 => values[single],
                    _ => builder.ins().iconst(ir::types::I32, 0),
                };
                builder.ins().return_(&[exit_value]);
                return;
            }
            match sret_ptr {
                Some(ptr) => {
                    let (offsets, ..) = data_layout::sequential_layout(return_types, arena);
                    for (i, inst) in instructions.iter().enumerate() {
                        builder.ins().store(MemFlags::trusted(), values[inst], ptr, offsets[i] as i32);
                    }
                    builder.ins().return_(&[]);
                },
                None => {
                    let mut vals = Vec::new();
                    for i in instructions {
                        vals.push(values[i]);
                    }
                    builder.ins().return_(&vals);
                },
            }
        },
        TerminatorInst::Unreachable => panic!("[internal error] reached an Unreachable terminator during translation"),
    }
}

pub fn translate_function(
    func_id: IrFuncId,
    arena: &IrArena,
    module: &mut dyn Module,
    func_ids: &HashMap<IrFuncId, ClifFuncId>,
    builtins_ctx: &BuiltinsContext,
) -> ir::Function {
    let func = arena.get_func(func_id);
    let sig = make_signature(func, arena, module);
    let user_func_name = ir::UserFuncName::user(0, func_ids[&func_id].as_u32());
    let mut clif_func = ir::Function::with_name_signature(user_func_name, sig);
    let mut fb_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut clif_func, &mut fb_ctx);

    let block_ids = arena.get_blocks_in(func_id);
    let mut clif_blocks = HashMap::new();
    for &block_id in &block_ids {
        clif_blocks.insert(block_id, builder.create_block());
    }

    let mut values: HashMap<InstId, ir::Value> = HashMap::new();
    let mut phi_order: HashMap<BlockId, Vec<InstId>> = HashMap::new();
    for &block_id in &block_ids {
        let clif_block = clif_blocks[&block_id];
        let mut phis = Vec::new();
        for &inst_id in &arena.get_block(block_id).instructions {
            if matches!(arena.get_inst(inst_id), Instruction::Phi { .. }) {
                let ty = infer_clif_type(inst_id, arena);
                values.insert(inst_id, builder.append_block_param(clif_block, ty));
                phis.push(inst_id);
            }
        }
        phi_order.insert(block_id, phis);
    }

    let return_types = return_type_ids(func, arena);
    let entry_clif_block = clif_blocks[&func.entry_block];
    builder.append_block_params_for_function_params(entry_clif_block);
    let translating_main = is_main(func);
    let sret_ptr = if !translating_main && uses_struct_return(&return_types) {
        Some(builder.block_params(entry_clif_block)[0])
    } else {
        None
    };
    let param_offset = if sret_ptr.is_some() { 1 } else { 0 };
    for (i, &param_inst) in func.parameter_insts.iter().enumerate() {
        values.insert(param_inst, builder.block_params(entry_clif_block)[i + param_offset]);
    }

    for &block_id in &block_ids {
        builder.switch_to_block(clif_blocks[&block_id]);
        for &inst_id in &arena.get_block(block_id).instructions.clone() {
            translate_instruction(inst_id, arena, &mut builder, module, func_ids, builtins_ctx, &mut values);
        }
        let term_id = arena.get_block(block_id).terminator.expect("[internal error] block with no terminator");
        translate_terminator(
            term_id,
            block_id,
            arena,
            &mut builder,
            &clif_blocks,
            &phi_order,
            &values,
            &return_types,
            sret_ptr,
            translating_main,
        );
    }

    builder.seal_all_blocks();
    builder.finalize();
    clif_func
}
