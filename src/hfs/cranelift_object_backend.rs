use std::{fs, path::Path, process::Command};

use cranelift_codegen::Context;
use cranelift_module::Module;
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::hfs::{
    IrArena,
    cranelift_builtins::declare_builtins,
    cranelift_translate::{declare_all_functions, translate_function},
    find_builtin,
};

pub fn compile_and_link(arena: &IrArena, output: &Path) -> Result<i32, String> {
    let isa_builder = cranelift_native::builder().map_err(|s| format!("unsupported host target: {s}"))?;
    let flags = cranelift_codegen::settings::Flags::new(cranelift_codegen::settings::builder());
    let isa = isa_builder.finish(flags).map_err(|e| format!("failed to configure cranelift target: {e}"))?;

    let obj_builder =
        ObjectBuilder::new(isa, "henceforth_module", cranelift_module::default_libcall_names())
            .map_err(|e| format!("failed to set up the object module: {e}"))?;
    let mut module = ObjectModule::new(obj_builder);

    let func_ids = declare_all_functions(arena, &mut module);
    let builtins_ctx = declare_builtins(&mut module);
    for (func_id, func) in arena.functions.iter() {
        if find_builtin(&func.name).is_some() {
            continue;
        }
        let clif_func = translate_function(func_id, arena, &mut module, &func_ids, &builtins_ctx);
        let mut ctx = Context::for_function(clif_func);
        module.define_function(func_ids[&func_id], &mut ctx).map_err(|e| format!("failed to compile a function: {e:?}"))?;
    }

    let obj_bytes = module.finish().emit().map_err(|e| format!("failed to emit the object file: {e}"))?;
    let obj_path = output.with_extension("o");
    fs::write(&obj_path, obj_bytes).map_err(|e| format!("failed to write {}: {e}", obj_path.display()))?;

    let result = link(&obj_path, output);
    let _ = fs::remove_file(&obj_path);
    result?;
    Ok(0)
}

fn link(obj_path: &Path, output: &Path) -> Result<(), String> {
    let linker = find_linker()?;
    let status = Command::new(&linker)
        .args([obj_path.as_os_str(), "-o".as_ref(), output.as_os_str(), "-no-pie".as_ref()])
        .status()
        .map_err(|e| format!("failed to run '{linker}': {e}"))?;
    if !status.success() {
        return Err(format!("'{linker}' failed to link {}", output.display()));
    }
    Ok(())
}

fn find_linker() -> Result<String, String> {
    for candidate in ["cc", "clang", "gcc"] {
        if Command::new(candidate).arg("--version").output().is_ok() {
            return Ok(candidate.to_string());
        }
    }
    Err("no system linker found (looked for cc, clang, gcc)".to_string())
}
