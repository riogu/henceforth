# Phase 1 — Cranelift AOT, scalar-only

Compiles scalar Henceforth programs (arithmetic, comparisons, `if`/`else`, `while`,
function calls, named-variable locals) to a real linked executable via
`cranelift-object` + the system `cc`. No arrays, strings, or builtins yet (Phase 3-5);
no wide multi-return (see below).

## What landed

- Cargo: `cranelift-codegen`/`-frontend`/`-module`/`-object`/`-native`, default-on
  `cranelift` feature (first `[features]` table in this crate).
- `src/hfs/cranelift_translate.rs`: `Instruction`/`TerminatorInst` → Cranelift IR.
  `Phi` becomes a Cranelift block param, with every jump/branch supplying the matching
  argument. Scalar `Alloca` → `stack_slot`. Function calls resolve real callees;
  builtins are name-matched and rejected with a clear "not yet" message (their
  `IrFunction` entries have no real body — see below).
- `src/hfs/cranelift_object_backend.rs`: builds an `ObjectModule`, emits a `.o`, links
  it with `cc`/`clang`/`gcc` (whichever's found) using `-no-pie`, writes the result to
  `-o`'s path.
- `src/hfs/backend.rs`: `BackendKind::Cranelift` now does the real thing.

## Verified

`phase1_smoke.hfs` (a function call inside a `while` loop summing 0..4 via a named
accumulator, then an `if`/`else`) compiles and runs correctly 20/20 times, exit code
42 as expected — and for `fn main: () -> (i32)`, the exit code *is* Cranelift's return
value with zero extra work, since a C-ABI `int main(void)` already treats its return
value as the exit code. Full existing test suite (176 tests across compile/error/opt)
still passes.

Scanned every `tests/compile_tests/*.hfs` fixture through `--backend cranelift`: things
using arrays/strings/builtins/tuple-element-access panic with a clear "not supported
yet, see Phase N" message, never a crash.

## Two real bugs found and fixed along the way

1. Builtins (`print`, `input_*`) have an `IrFunction` entry for their signature only —
   no real body. The driver loop was translating them anyway and hitting garbage
   block/instruction data. Fixed by skipping any function `find_builtin` recognizes,
   in both declaration and translation.
2. A parameter that's immediately discarded (`@pop` right after binding) can get
   dropped entirely by DCE, leaving `IrFunction.parameter_insts` pointing at a deleted
   instruction. Signature-building used to read each parameter's type off its own
   `Instruction::Parameter`, which crashed on this. Fixed by reading `param_type`/
   `return_type` instead (always a `Tuple`, and untouched by DCE) — the only reliable
   source, and it's what return types were already using.

## Four things that turned out not to be Cranelift bugs — all fixed (separate commits)

Found while testing against real fixtures, pre-existing in `ir_lowerer.rs`/
`ir_optimizations.rs`/`interpreter.rs`, unrelated to the Cranelift translator itself:

1. **`while` loops didn't build anything for a value carried across iterations on the
   stack** (only `if`/`else` merges did, via `generate_merge_phis`) — `pow.hfs` hit
   this: its accumulator lived on the stack, not in a named variable, across the loop,
   and the multiply always read the *pre-loop* value, lowered once textually. This
   wasn't just a dominance technicality: `pow(2, 10)` actually computed `2`, not
   `1024`, under the interpreter too — nothing had ever checked its output. Fixed by
   routing the value through a real alloca/store/load, exactly like a `let` variable
   gets, instead of hand-building a phi — Mem2Reg already places a correct phi for an
   arbitrary CFG, `continue` included, so this reuses that rather than duplicating a
   weaker version of it. See the `ir_lowerer.rs` commit for the before/after IR and why
   the first attempt at this (a hand-built incomplete phi) got `continue` wrong.
2. **`comprehensive-else-if.hfs` compiled to a genuinely different result across
   separate runs of the identical binary on identical input.** Root cause:
   `Mem2Reg::rename_variables`'s dominator-tree-sibling snapshot/restore only
   snapshotted allocas that already had an entry — a variable whose first `Store`
   happened inside whichever sibling subtree got visited first (order controlled by
   iterating a `HashMap`) leaked its value into unrelated siblings, occasionally
   producing a phi that referenced itself. Fixed — see the `ir_optimizations.rs`
   commit.
3. **The interpreter infinite-recursed on a loop-header phi that legitimately
   references itself** (a value updated in one loop branch but not another, combined
   with `continue`, needs an edge saying "unchanged on this path" — that's itself, and
   it's valid SSA since the header dominates its own back-edge; Cranelift has no issue
   with it). The interpreter unconditionally clears every instruction's cache on each
   block visit, so resolving a phi back to itself just re-triggered the same
   resolution forever. Fixed by not invalidating a phi's cache on the specific visit
   where its own value is what the incoming edge says to use. See the
   `interpreter.rs` commit.
4. **Nesting two `while` loops that share a counter and each carry their own
   stack-based accumulator crashed the interpreter** with "reached phi without going
   through one of its predecessor blocks" — found by hand-editing
   `while_stack_value_continue.hfs` into a nested-loop shape. Root cause:
   `CleanCFG`'s empty-block removal rewires a removed block's predecessors to jump
   straight to its jump target, but never updated that target's phi instructions to
   swap the removed block's incoming edge for one edge per predecessor it just
   rewired. Fixed — see the `ir_optimizations.rs` commit (a different one from #2,
   same file).

All four now reproduce identically across repeated runs (interpreter and Cranelift),
and are covered by three fixtures: `while_stack_value_continue.hfs`,
`while_conditional_update_continue.hfs`, and `nested_while_shared_counter.hfs`.

Also hit a real Cranelift ABI limit: a function returning 6 `i32`s errors with "too
many return values to fit in registers, use a StructReturn argument instead" — wide
multi-value returns need `sret`, not the plain multi-value `return` Phase 1 uses.
Relevant for Phase 2's calling-convention work.

## Next

Phase 2: real multi-param/multi-return calling convention (including the sret case
above), replacing the `ReturnValue` interpreter-only placeholder binding with the
compile-time version.
