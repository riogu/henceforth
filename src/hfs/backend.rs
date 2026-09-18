use std::path::PathBuf;

use crate::hfs::{Interpreter, IrArena, IrTopLevelId, LinkOptions, ScopeStack, cranelift_object_backend};

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum BackendKind {
    /// Walks the IR directly without compiling to native code
    Interpret,
    /// Compiles to native code using the Cranelift backend
    Cranelift,
}

pub fn run(
    kind: BackendKind,
    mut arena: IrArena,
    top_level_insts: Vec<IrTopLevelId>,
    scope_stack: ScopeStack,
    output: Option<PathBuf>,
    link_opts: &LinkOptions,
) -> i32 {
    match kind {
        BackendKind::Interpret => {
            Interpreter::interpret(arena, top_level_insts, scope_stack);
            0
        },
        BackendKind::Cranelift => {
            let output = output.expect("[internal error] --backend cranelift requires an output path");
            match cranelift_object_backend::compile_and_link(&mut arena, &output, link_opts) {
                Ok(code) => code,
                Err(e) => {
                    eprintln!("cranelift backend error: {e}");
                    1
                },
            }
        },
    }
}
