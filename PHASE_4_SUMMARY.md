# Phase 4 — String ABI

`IrType::String` now has a real Cranelift representation, and `print`/`input_str` work with it.

## Representation

A Henceforth string is a fat `{ptr: i64, len: i64}` value (per the plan's already-resolved ABI
decision). Cranelift's `i128` is exactly that width, so a string value is represented as one
`i128` (`ptr` in the low 64 bits, `len` in the high 64 bits via `iconcat`/`isplit`) rather than
as a separate two-`Value` case threaded through the translator.

This turned out to make the rest of the translator's genuinely mechanical, as expected going
into this phase: `ir_type_to_clif(String) = I128` and `data_layout::size_of(String) = 16`
(already true) are the only two facts every other generic path needs - `Alloca`/`Load`/`Store`,
block params (phi), function params/returns, call args/results, and the `StructReturn` buffer
layout all already worked off `type_id`/`ir_type_to_clif` and needed no change at all. Only three
places actually touch strings directly: string literals become static rodata (`ptr`) plus their
byte length (`len`), `print` picks a format based on the value's inferred Cranelift type (already
existing machinery, from Phase 3), and `input_str` is new.

## input_str

Uses `getline` (grows its own buffer as needed) rather than a fixed-size `fgets` buffer, so an
arbitrarily long line reads correctly instead of silently truncating - matching the interpreter,
which has no length limit either. `getline` needs the `stdin` `FILE*`, pulled in as an imported
extern data symbol (glibc exports it as a plain global, not a macro).

On EOF/error `getline` returns `-1` without necessarily having allocated a buffer, so the string's
pointer is only trusted when the call actually succeeded; trimming a trailing `\n` and `\r` (two
byte-inspection loads against the returned buffer, matching the interpreter's
`trim_end_matches(['\n', '\r'])`) is done entirely with `select` - branchless, but every load is
still gated so it only ever reads a real, in-bounds byte, never a null or leftover pointer.

## The unrelated bug this surfaced: main's exit code was garbage

Testing `input_str` against an empty stdin (immediate EOF) produced exit code 4 from the AOT
binary against exit code 0 from the interpreter - traced to any `main: () -> ()` program, not
just this one. The C runtime calls the linked `main` as `int main(void)` and uses whatever's left
in the return register as the exit code; a Henceforth `main` with no `i32` return compiled to a
Cranelift signature with zero returns, which correctly never touches that register - so the
"exit code" was just whatever the function's last computation happened to leave behind. This is
exactly the behavior Phase 1's plan already called for ("if main's return type includes an Int,
use it as exit code; otherwise exit 0") but was never actually implemented. Fixed by pinning
`main`'s Cranelift signature to always return a single `i32`, and having its `Return` translation
supply that from Henceforth's own return value when it's a lone `i32`, or a literal `0` otherwise.

## Verification

- `print("a literal string")` and `print(42)` in the same program produce output
  byte-for-byte identical to the interpreter.
- `input_str` round-tripped through `print`, checked against a plain `\n` ending, a `\r\n`
  ending, no trailing newline at all (EOF right after content), and immediate EOF (empty
  stdin) - all match the interpreter's output and exit code.
- `print_any_type.hfs` (string print now succeeds; array print still panics "see Phase 5" as
  expected) and a previously-void `main` (`factorial.hfs`) both now exit 0 correctly.
- New fixture `tests/compile_tests/print_and_input_str.hfs`.
- Full suite: 103 error + 26 compile tests + 1 opt, all passing, no warnings, no regressions.

## Status

Phase 4 appears complete per the plan's own scope.
