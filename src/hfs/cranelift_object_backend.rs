use std::{collections::HashMap, fs, path::Path, process::Command};

use cranelift_codegen::Context;
use cranelift_module::Module;
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::hfs::{
    InstId, IrArena, IrFuncId,
    cranelift_builtins::declare_builtins,
    cranelift_translate::{declare_all_functions, translate_function},
    find_builtin, ir_aggregate_lowering,
};

#[derive(Debug, Clone, Default)]
pub struct LinkOptions {
    pub compile_only: bool,
    pub keep_obj: bool,
    pub libs: Vec<String>,
    pub lib_dirs: Vec<String>,
    pub linker: Option<String>,
    pub strip: bool,
    pub static_link: bool,
    pub verbose: bool,
}

pub fn compile_and_link(arena: &mut IrArena, output: &Path, opts: &LinkOptions) -> Result<i32, String> {
    let mut array_stores: HashMap<IrFuncId, HashMap<InstId, u32>> = HashMap::new();
    for func_id in arena.functions.clone().keys() {
        let func = arena.get_func(func_id);
        if find_builtin(&func.name).is_some() || func.is_extern {
            continue;
        }
        array_stores.insert(func_id, ir_aggregate_lowering::legalize_arrays(arena, func_id));
    }

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
        if find_builtin(&func.name).is_some() || func.is_extern {
            continue;
        }
        let clif_func = translate_function(func_id, arena, &mut module, &func_ids, &builtins_ctx, &array_stores[&func_id]);
        let mut ctx = Context::for_function(clif_func);
        module.define_function(func_ids[&func_id], &mut ctx).map_err(|e| format!("failed to compile a function: {e:?}"))?;
    }

    let obj_bytes = module.finish().emit().map_err(|e| format!("failed to emit the object file: {e}"))?;
    let obj_path = if opts.compile_only { output.to_path_buf() } else { output.with_extension("o") };
    fs::write(&obj_path, obj_bytes).map_err(|e| format!("failed to write {}: {e}", obj_path.display()))?;

    if opts.compile_only {
        return Ok(0);
    }

    let result = link(&obj_path, output, opts);
    if !opts.keep_obj {
        let _ = fs::remove_file(&obj_path);
    }
    result?;
    Ok(0)
}

fn link(obj_path: &Path, output: &Path, opts: &LinkOptions) -> Result<(), String> {
    let linker = find_linker(opts)?;

    let mut args: Vec<std::ffi::OsString> =
        vec![obj_path.as_os_str().into(), "-o".into(), output.as_os_str().into(), "-no-pie".into()];
    for dir in &opts.lib_dirs {
        args.push(format!("-L{dir}").into());
    }
    for lib in &opts.libs {
        args.push(format!("-l{lib}").into());
    }
    if opts.strip {
        args.push("-s".into());
    }
    if opts.static_link {
        args.push("-static".into());
    }

    if opts.verbose {
        eprintln!("{linker} {}", args.iter().map(|a| a.to_string_lossy()).collect::<Vec<_>>().join(" "));
    }

    let status = Command::new(&linker).args(&args).status().map_err(|e| format!("failed to run '{linker}': {e}"))?;
    if !status.success() {
        return Err(format!("'{linker}' failed to link {}", output.display()));
    }
    Ok(())
}

fn find_linker(opts: &LinkOptions) -> Result<String, String> {
    if let Some(linker) = &opts.linker {
        return Ok(linker.clone());
    }
    for candidate in ["cc", "clang", "gcc"] {
        if Command::new(candidate).arg("--version").output().is_ok() {
            return Ok(candidate.to_string());
        }
    }
    Err("no system linker found (looked for cc, clang, gcc)".to_string())
}
