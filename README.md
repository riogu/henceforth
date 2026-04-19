# henceforth

A statically-typed stack-based programming language exploring middle-end compiler design with SSA-form IR, CFG construction and dataflow-driven optimization.

## Example

```rust
fn print: (str) -> () { @pop } // this is an intrinsic (satisify the compiler for now)
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
    if @(@dup 120 ==) {
        @("factorial example\n") &> print;
    }
    @pop
}
```

Stack blocks (`@(...)`) make data flow explicit. Values are pushed, manipulated, and consumed through stack operations rather than implicit variable binding. All branches must leave the stack in a consistent state, enforced at compile time.

## Language

**Types:** `i32`, `f32`, `bool`, `str`, pointers, and tuple types for function signatures.

**Stack operations:** `@(expr)` pushes values, `@dup` duplicates, `@pop` discards, `@depth` introspects. `:=`/`:>` copy, `&=`/`&>` move.

**Control flow:** `if`/`else if`/`else`, `while` loops with `break`/`continue`. Stack depth and types are verified across all control flow paths at compile time.

**Functions:** Stack-based signatures `(params) -> (returns)` with function-scoped stacks.

## Compiler Pipeline

```
Source → Lexer → Parser → Stack Analyzer → CFG Analyzer → MIR → Optimizer → Interpreter
```

The frontend is hand-written: lexer, recursive descent parser, two-pass stack analyzer for identifier resolution and type checking. The CFG analyzer lowers the AST to MIR, validating stack depth and types at control flow boundaries.

## Middle-End

The compiler's middle-end operates on an SSA-form MIR with explicit control flow graphs, backed by a generational arena (SlotMap) for stable instruction references across optimization passes.

### Analysis Infrastructure

- **Def-use chains:** per-function use-def computation with user tracking, RAUW support, and incremental user removal for worklist algorithms.
- **Dominator tree:** Cooper-Harvey-Kennedy iterative algorithm, with dominance frontier computation for phi placement.
- **Reverse postorder** traversal for forward dataflow analysis.

### Optimization Passes

- **Mem2Reg:** SSA construction via iterated dominance frontier phi insertion and dominator-tree-driven renaming. Promotes alloca/load/store chains to SSA values with phi nodes.
- **Dead code elimination:** worklist-driven DCE using def-use chains. Removes unused instructions and propagates liveness through operands.
- **Stale ID cleanup:** removes invalidated instruction references from basic blocks after transformation passes.

### Planned

- Aggressive dead code elimination (ADCE) using post-dominance and control dependence.
- CFG simplification (redundant branch folding, empty block removal, block merging).
- Constant propagation / SCCP.
- Global value numbering.
- Loop-invariant code motion.
- Backend targeting Cranelift for native code generation.

## Documentation

Language specification and usage docs at [riogu.github.io/henceforth](https://riogu.github.io/henceforth).
