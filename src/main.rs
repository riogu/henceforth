#![allow(unused)]

use std::{error::Error, path::PathBuf, process::exit, rc::Rc};

use clap::{Parser, arg};
use henceforth::hfs::{
    self, LinkOptions, OptPipeline,
    error::{CompileError, DiagnosticInfo},
    get_eof_span,
    ir_lowerer_errors::IrLowererError,
};

#[derive(Parser, Debug, Clone)]
#[command(
    version,
    about = "An optimizing compiler for an imperative stack-based language",
    after_help = "Example:\n  henceforth main.hfs -o main.out"
)]
struct Args {
    /// Path to the Henceforth source file to compile
    source: PathBuf,
    /// Path to write the compiled output to. Ignored if backend is set to 'interpret'
    #[arg(short, long, default_value = "./a.out")]
    output: PathBuf,
    /// Which backend to use for execution
    #[arg(long, value_enum, default_value = "cranelift")]
    backend: hfs::BackendKind,
    /// Print the IR before optimizations are applied
    #[arg(long = "print-ir-O0")]
    print_ir_o0: bool,
    /// Print the IR after optimizations are applied
    #[arg(long)]
    print_ir: bool,
    /// Write the IR before optimizations to a .O0.hfsir file
    #[arg(long = "emit-ir-O0")]
    emit_ir_o0: bool,
    /// Write the IR after optimizations to a .hfsir file
    #[arg(long)]
    emit_ir: bool,
    /// Print the input source file to the terminal
    #[arg(long)]
    print_file: bool,
    /// Compile only, do not link (produces a .o file at the output path)
    #[arg(short = 'c')]
    compile_only: bool,
    /// Keep the intermediate .o object file alongside the linked binary
    #[arg(long)]
    emit_obj: bool,
    /// Add directory to library search path (passed to the linker as -L<dir>)
    #[arg(short = 'L', value_name = "directory")]
    lib_dirs: Vec<String>,
    /// Link with library (passed to the linker as -l<library>)
    #[arg(short = 'l', value_name = "library")]
    libs: Vec<String>,
    /// Specify which linker to use (default: auto-detect cc/clang/gcc)
    #[arg(long = "linker", value_name = "linker")]
    linker: Option<String>,
    /// Strip symbol table from the executable (passed to the linker as -s)
    #[arg(short = 's')]
    strip: bool,
    /// Create a statically linked executable (passed to the linker as -static)
    #[arg(long = "static")]
    static_link: bool,
    /// Show the linker command that was executed
    #[arg(short = 'v')]
    verbose: bool,
}

fn run() -> Result<i32, Box<dyn CompileError>> {
    let args = Args::parse();
    let file = hfs::File::new(args.source);
    let file_name = file.path.to_str().unwrap().to_string();

    if args.print_file {
        println!("{}", file.contents.join("\n"));
    }

    let tokens = hfs::Lexer::tokenize(&file)?;
    let diagnostic_info = Rc::new(DiagnosticInfo::new(file.path, get_eof_span(&tokens)));

    let (unresolved_top_level_nodes, unresolved_ast_arena) = hfs::Parser::parse_tokens(tokens.clone(), diagnostic_info.clone())?;

    let (top_level_nodes, ast_arena, scope_stack) =
        hfs::StackAnalyzer::resolve(unresolved_top_level_nodes, unresolved_ast_arena.clone(), diagnostic_info.clone())?;

    let (top_level_insts, mut ir_arena) =
        hfs::IrLowerer::lower_to_mir(top_level_nodes, ast_arena.clone(), diagnostic_info.clone())?;

    if args.print_ir_o0 {
        println!("IR before optimizations:{}", IrLowererError::dump_ast_and_ir(None, &ir_arena));
    }
    if args.emit_ir_o0 {
        let path = diagnostic_info.path.with_extension("O0.hfsir");
        std::fs::write(&path, IrLowererError::dump_ast_and_ir(None, &ir_arena))
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", path.display()));
    }

    hfs::OptPipeline::run_iteratively(&mut hfs::O0::new(), &mut ir_arena);

    if args.print_ir {
        println!("IR after optimizations:{}", IrLowererError::dump_ast_and_ir(None, &ir_arena));
    }
    if args.emit_ir {
        let path = diagnostic_info.path.with_extension("hfsir");
        std::fs::write(&path, IrLowererError::dump_ast_and_ir(None, &ir_arena))
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", path.display()));
    }

    let link_opts = LinkOptions {
        compile_only: args.compile_only,
        keep_obj: args.emit_obj,
        libs: args.libs,
        lib_dirs: args.lib_dirs,
        linker: args.linker,
        strip: args.strip,
        static_link: args.static_link,
        verbose: args.verbose,
    };
    Ok(hfs::backend::run(args.backend, ir_arena, top_level_insts, scope_stack, Some(args.output), &link_opts))
}

fn main() {
    match run() {
        Ok(exit_code) => exit(exit_code),
        Err(e) => {
            eprintln!("{}", e);
            exit(1);
        },
    }
}
