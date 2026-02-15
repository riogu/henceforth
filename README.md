# henceforth

A statically-typed stack-based programming language exploring middle-end compiler design — SSA-form IR, CFG construction, and dataflow-driven optimization.

## Example

```rust
fn factorial: (i32) -> (i32) {
    let n: i32;
    let result: i32;

    &= n;
    @(1) &= result;

    while @(n 1 >) {
        @(result n *) &= result;
        @(n 1 -) &= n;
    }

    @(result);
}

fn main: () -> () {
    @(5) &> factorial;
    @(120 ==);
}
```

Stack blocks (`@(...)`) make data flow explicit. Values are pushed, manipulated, and consumed through stack operations rather than implicit variable binding.

## Language

**Types:** `i32`, `f32`, `bool`, `str`, tuple types for function signatures.

**Stack operations:** `@(expr)` pushes values, `@dup` duplicates, `@pop` discards, `@depth` introspects. `:=`/`:>` copy, `&=`/`&>` move.

**Control flow:** `if`/`else if`/`else`, `while` loops. All branches must leave the stack in a consistent state — enforced at compile time.

**Functions:** Stack-based signatures `(params) -> (returns)` with function-scoped stacks.

## Compiler Pipeline

```
Source → Lexer → Parser → Stack Analyzer → CFG → MIR → (planned) LLVM IR
                                                    ↓
                                                Interpreter
```

The frontend is hand-written: lexer, recursive descent parser, two-pass stack analyzer for identifier resolution and type checking. The CFG analyzer validates stack depth and types at control flow boundaries.

MIR is the current focus — an SSA-form intermediate representation with CFG structure, designed as the target for dataflow analysis and optimization passes.

## Middle-End (in progress)

The compiler's middle-end is being built around an SSA-form MIR with explicit control flow graphs. Current and planned work:

**IR construction:** SSA construction using Braun et al.'s algorithm (on-the-fly phi insertion during IR generation). CFG with basic blocks, explicit predecessor/successor edges.

**Dataflow analysis infrastructure:** Iterative fixed-point solver operating over the CFG. Planning to implement liveness analysis, available expressions, and reaching definitions as the foundation for optimization passes.

**Optimization passes (planned):**
- Dead code elimination via liveness / SSA use-lists
- Constant propagation (sparse conditional constant propagation on SSA)
- Loop-invariant code motion — studying LLVM's approach using dominance, alias analysis, and MemorySSA rather than classical LCM
- Global common subexpression elimination via value numbering on SSA
- Strength reduction for induction variables

**Backend (planned):** LLVM IR generation from optimized MIR.

## Project Structure

```
src/hfs/
├── lexer.rs              # Tokenization
├── token.rs              # Token definitions
├── parser.rs             # Recursive descent parser, produces unresolved AST
├── unresolved_ast.rs     # First-pass AST before name resolution
├── stack_analyzer.rs     # Stack analysis, identifier resolution, type checking
├── ast.rs                # Resolved AST
├── scope_stack.rs        # Symbol table / scope resolution
├── types.rs              # Type system
├── cfg_analyzer.rs       # CFG construction and stack validation
├── hfs_mir.rs            # SSA-form mid-level IR
├── interpreter.rs        # MIR interpreter
├── ast_interpreter.rs    # AST tree-walking interpreter
├── diagnostics/          # Error reporting
└── builder/              # Test infrastructure
```

## Documentation

Language specification and usage docs at [riogu.github.io/henceforth](https://riogu.github.io/henceforth).
