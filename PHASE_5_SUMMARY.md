# Phase 5 — array legalization

New `src/hfs/ir_aggregate_lowering.rs`, plus array-aware handling added to `cranelift_translate.rs`.

## What's arena-level vs. inline in the translator

The plan asked for this as a distinct IR-to-IR pass rather than inline in the translator, mainly
so the interpreter's own (already-correct) handling of `GetElementPtr`/`Alloca` stays completely
untouched and so this stays independently inspectable/testable later. That's exactly what
`ir_aggregate_lowering.rs` does for the one thing that's a genuine semantic rewrite: every
`GetElementPtr{address, indexes: [idx], type_id}` becomes plain `Operation::Add`/`Mul`
instructions computing `address + idx * size_of(type_id)` (reusing the GEP's own `InstId` for the
final `Add`, so nothing downstream needs to change) - by the time `cranelift_translate.rs` runs,
there is no more "logical index" concept anywhere in the arena, only ordinary pointer arithmetic.
Mixing a 32-bit index with the 64-bit pointer it offsets needed one small generic fix in
`translate_operation`: integer `Add`/`Sub`/`Mul` now widen the narrower operand to match instead
of assuming both sides were always the same width (true of every other operand pair until now).

The other two items ended up as straightforward extensions of `Alloca`/`Load`/`Store`'s *existing*
translation instead of new rewrites, once arrays got the same "a value is just its address"
treatment Cranelift already gives every other type by default:

- **Allocation strategy**: `Alloca`'s existing code already handles a constant-length array's
  `stack_slot` for free (`data_layout::size_of` already did the math). The only new branch is for
  a non-constant `array_len` (a parameter, not a literal) - it can't size a `stack_slot` at
  compile time, so it calls `malloc(len * elem_size)` instead and uses the returned pointer.
- **Whole-array copy**: a `Load`/`Store` of array type never had a coherent single-register
  Cranelift value to load/store in the first place, so `Load` of an array is just a pass-through
  of its address (nothing to load), and `Store` of an array becomes a `memcpy` call sized off the
  *destination*'s own declared length (a decayed/unsized parameter as the source carries no
  length of its own here, only a pointer - matching the interpreter, which also gets the shape
  from the destination alloca, not the source value).

## Free-on-return

The plan flagged this placement as an open question ("`stack_analyzer.rs` is a reasonable
alternative location; final call is the user's"), anticipating it would need lowering-time,
backend-gated bookkeeping. It didn't end up needing that: every local's `Alloca` always runs
unconditionally in the function's entry block regardless of which branch is taken later (an
existing convention, to keep Mem2Reg simple) - so the set of heap arrays live at *any* `Return`
in a function is just "every dynamic-length array `Alloca` in this function," full stop, no
per-path liveness analysis needed. `translate_function` computes that list once; `Return`
translation frees every one of them except whichever is being returned (resolved by walking back
through array `Load` pass-throughs to the underlying `Alloca` - a decayed parameter resolves to
nothing, correctly, since this function never allocated it).

Verifying `bubble_sort` under valgrind surfaced one more real leak, since ownership can also
pass through *unnamed* values: a function call's heap-array return value, used exactly once as
the source of a whole-array copy and never bound to a local of its own, had nothing tracking it.
Fixed by freeing a `ReturnValue`-typed copy source right after the `memcpy` consumes it - a
`ReturnValue` is by design a single-use landing slot for a call's result (see its own doc
comment), so this is always its only use.

**Known gaps, not fixed here**: a heap array returned from a call and then discarded outright
(`@pop`, never bound to anything) still leaks - nothing hooks into a bare discard the way the
whole-array-copy path does. A function *returning a fixed-size local array by value* would also
return a dangling stack address (its frame is gone by the time the caller reads it) - no existing
code does this (every array-by-value case in practice is a decayed/heap-backed parameter or
return, matching C's own convention), so it's undiscovered rather than fixed. Also: verifying
this surfaced that `input_str` (Phase 4) leaks its `getline` buffer - strings have no ownership
tracking at all, in either phase's design; worth its own decision later rather than folding an
unplanned string ownership model into this phase.

## Verification

- Constant-size local array (`array_indexing.hfs`'s shape): element read/write through a real
  AOT binary, byte-for-byte identical output to the interpreter.
- `bubble_sort`'s exact shape (parameter length, `[N]i32` malloc'd local, whole-array copy from a
  decayed parameter, in-place element swaps, returning the sorted array, caller copying it back
  into a fixed-size local): sorts correctly, matches the interpreter, deterministic across
  repeated compiles.
- `valgrind --leak-check=full` on both: zero leaks, zero errors, alloc/free counts balanced
  (2/2 for `bubble_sort`, including the ReturnValue fix above).
- Full suite: 103 error + 26 compile tests + 1 opt, all passing, no warnings, no regressions.
- Array `print` (never in scope - the plan never asked for it) still panics cleanly rather than
  miscompiling.

## Status

Phase 5 appears complete per the plan's own scope, with the two known gaps above called out for
awareness rather than silently left in.
