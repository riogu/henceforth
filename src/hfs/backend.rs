use std::path::PathBuf;

use crate::hfs::{Interpreter, IrArena, IrTopLevelId, ScopeStack, cranelift_object_backend};

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum BackendKind {
    Interpret,
    Cranelift,
}

pub fn run(kind: BackendKind, arena: IrArena, top_level_insts: Vec<IrTopLevelId>, scope_stack: ScopeStack, output: Option<PathBuf>) -> i32 {
    match kind {
        BackendKind::Interpret => {
            Interpreter::interpret(arena, top_level_insts, scope_stack);
            0
        },
        BackendKind::Cranelift => {
            let output = output.expect("[internal error] --backend cranelift requires an output path");
            match cranelift_object_backend::compile_and_link(&arena, &output) {
                Ok(code) => code,
                Err(e) => {
                    eprintln!("cranelift backend error: {e}");
                    1
                },
            }
        },
    }
}
