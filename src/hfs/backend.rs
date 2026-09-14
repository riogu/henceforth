use std::path::PathBuf;

use crate::hfs::{Interpreter, IrArena, IrTopLevelId, ScopeStack};

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
        // filled in starting Phase 1
        BackendKind::Cranelift => {
            let _ = output;
            eprintln!("the cranelift backend is not implemented yet");
            1
        },
    }
}
