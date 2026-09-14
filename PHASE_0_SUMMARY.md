# Phase 0 — Cranelift backend: scaffolding

Part of the Cranelift AOT backend plan. No codegen yet — this just makes room for a
second backend in the CLI/pipeline.

## What landed

- `src/hfs/backend.rs`: `BackendKind::{Interpret, Cranelift}` and `backend::run(...)`,
  the dispatch point between backends. `Cranelift` is a stub for now (prints a message,
  exits 1) — real codegen starts in Phase 1.
- `src/hfs/data_layout.rs`: `size_of`/`align_of`/`const_array_len` over `IrType`. Fixed
  primitive sizes (`Int`/`Float`/`Bool` = 4/4/1 bytes, pointers = 8, `String` = 16 for
  its future `{ptr, len}` fat-value layout) plus `index * element size` for
  constant-length arrays. Not used by anything yet; wired up starting Phase 5.
- `src/main.rs`: `-o/--output`, `--backend <interpret|cranelift>`, and
  `--print-ir-pre-opt`/`--print-ir-post-opt` (the latter two replace the commented-out
  debug `println!`s that were already in the file). Default backend stays `interpret`
  until Cranelift actually works end-to-end.

## Found and reverted along the way

The plan called for fixing `utils.rs`'s `Phase::Interpreter` arm in `run_until`, which
was a no-op (`Ok(Rc::new(()))`) instead of actually calling the interpreter. Turns out
that's intentional, not a bug: `tests/compile_tests.rs` runs every `compile_tests`
fixture through `Phase::Interpreter`, and a couple of those fixtures are designed to
only check that compilation succeeds — `new_whiles.hfs` loops forever by design, and
`builtin_input.hfs`/`tictactoe-user-input.hfs` block on stdin. Making the interpreter
actually run there hung the test suite. Left it as a no-op; running these fixtures for
real belongs with Phase 6's parity testing, which already needs curated fixtures and
stdin/stdout handling.

## Verification

`cargo build` clean, `cargo test` — 20 compile_tests, 103 error_tests, 1 opt_test, 53
lib unit tests, all passing. Manually ran a fixture through `--backend interpret`
(unchanged behavior) and `--backend cranelift` (clean "not implemented yet", exit 1),
and confirmed `--print-ir-pre-opt`/`--print-ir-post-opt` print real IR.

## Next

Phase 1: minimal AOT — scalar-only `main`, real Cranelift dependency, produces an
actual linked executable.
