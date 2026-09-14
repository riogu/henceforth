# Phase 3 — builtins via libc

New `src/hfs/cranelift_builtins.rs`, wired into `cranelift_translate.rs`'s `FunctionCall`
handling in place of the Phase-3 placeholder panic.

## Scope

- `print` for `i32`/`f32`/`bool` (matching the interpreter's own formatting: no trailing
  newline, `Bool` as `"true"`/`"false"` rather than `0`/`1`).
- `input_int`/`input_float` via `scanf`.
- `print` on a `str`, and `input_str`, still panic cleanly ("see Phase 4") - strings have no
  Cranelift representation yet regardless of builtins, so this falls out of the existing
  `ir_type_to_clif`/`infer_clif_type` panics as soon as a string value is touched.

## The one real design problem: printf/scanf are variadic, Cranelift signatures aren't

Each `print` call site needs a different fixed-arity signature depending on what's being
printed (`(ptr, i32)` for an int, `(ptr, f64)` for a float after promotion, `(ptr, ptr)` for a
bool-as-string) - but `cranelift_module::Module::declare_function` keys declarations by name
and rejects redeclaring the same symbol under a different signature
(`ModuleError::IncompatibleSignature`). So `printf`/`scanf` are each declared exactly once,
under a placeholder signature that's only ever used to obtain their address; every real call
builds its own `ir::Signature` locally (`FunctionBuilder::import_signature`), gets the callee's
address via `func_addr`, and calls through it with `call_indirect`. This is the same technique
other Cranelift frontends use for calling C variadic functions.

Two other C ABI details fall out of this: a `float` passed to a variadic function is always
widened to `double` at the call site (`fpromote`) - this is a C-language rule the callee relies
on via `va_arg`, not something the target ABI does automatically - and `bool` picks between two
static `"true"`/`"false"` C strings with a `select` on the 0/1 value rather than a branch.

`input_int`/`input_float` zero their scratch stack slot before calling `scanf`, matching the
interpreter's own "malformed input becomes 0/0.0" fallback instead of leaving it holding
whatever `scanf` didn't write on failure.

## Verification

No JIT and no backend-parity harness yet (that's Phase 6), so this was checked the same way as
Phase 1/2: real AOT binaries compiled and run directly.

- `print` of `42`, `3.5`, `true`, `false` produces `423.5truefalse` (no newlines) - byte-for-byte
  identical to the interpreter's own output for the same program.
- `input_int`/`input_float` round-tripped through `print` match the interpreter given the same
  stdin.
- `print_any_type.hfs` and `builtin_input.hfs` (both pre-existing, both touch `str`) compiled
  through the Cranelift backend still fail with the expected "see Phase 4" panic rather than
  crashing some other way.
- New fixture `tests/compile_tests/print_scalars.hfs` covers `print`/`input_int`/`input_float`
  together for permanent compile-level coverage (this and the existing fixtures only exercise
  `Phase::Interpreter`, per `compile_tests.rs` - not the Cranelift path itself).

Full suite: 103 error tests + 25 compile tests (one new: `print_scalars.hfs`) + 1 opt test, all
passing, no regressions, no warnings.

## Status

Phase 3 appears complete per the plan's own scope.
