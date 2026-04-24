pub mod hfscheck {
    pub mod error_parser;
    pub mod hfscheck;
    pub mod ir_check;
}

pub mod hfs {
    pub mod ast;
    // pub mod ast_interpreter;
    pub mod diagnostics;
    pub mod hfs_ir;
    pub mod interpreter;
    pub mod ir_analysis;
    pub mod ir_arena;
    pub mod ir_lowerer;
    pub mod ir_optimizations;
    pub mod ir_pretty_printing;
    pub mod ir_syntax;
    pub mod lexer;
    pub mod parser;
    pub mod scope_stack;
    pub mod stack_analyzer;
    pub mod token;
    pub mod types;
    pub mod unresolved_ast;
    pub mod utils;
    pub use ast::*;
    pub use diagnostics::*;
    pub use hfs_ir::*;
    pub use interpreter::*;
    pub use ir_analysis::*;
    pub use ir_arena::*;
    pub use ir_lowerer::*;
    pub use ir_optimizations::*;
    pub use ir_pretty_printing::*;
    pub use ir_syntax::*;
    pub use lexer::*;
    pub use parser::*;
    pub use scope_stack::*;
    pub use stack_analyzer::*;
    pub use token::*;
    pub use types::*;
    pub use unresolved_ast::*;
    pub use utils::*;
}
