# Phase 2 — calling convention

Per the plan, Phase 2's three items were: multi-value `Return` translation, `FunctionCall`
binding `return_values` from call results, and params from `block_params`. All three
already existed in `cranelift_translate.rs` from Phase 1 — `Return`'s `return_tuple` is
always a `Tuple` regardless of arity, so multi-value `return_` fell out for free once
single-value returns worked, and params/call-result binding were never single-value-only
to begin with.

The one real gap Phase 1 testing surfaced: Cranelift has a hard limit on how many return
values fit in registers (`comprehensive-else-if.hfs`'s 6-`i32` return hit it: "Too many
return values to fit in registers. Use a StructReturn argument instead."). Implemented
`StructReturn` properly (not the deprecated `enable_multi_ret_implicit_sret` flag, which
doesn't conform to platform ABIs and is slated for removal):

- Any function with **more than one** return value gets a hidden pointer parameter
  (`ArgumentPurpose::StructReturn`, placed first) instead of native returns — uniformly,
  even for arities that would technically fit in registers. Simpler than replicating each
  target's exact register budget, and correct on all of them.
- The callee stores each return value into that buffer at its own naturally-aligned offset
  (`data_layout::sequential_layout`, new: lays out heterogeneous values like an unpacked
  struct) instead of doing a multi-value `return`.
- The caller allocates a stack slot sized for the whole buffer, passes its address as the
  extra leading argument, and loads each value back out afterward instead of reading call
  results directly.

Verified with a 3-`i32` return and a mixed `i32`/`bool`/`i32`/`bool` return (exercises the
alignment math, not just size) through the real Cranelift AOT binary — both give the
expected value. `comprehensive-else-if.hfs` now compiles cleanly, 20/20 runs, no more ABI
error. Both new fixtures (`multi_return_struct_return.hfs` for permanent coverage, plus
manual scratch tests) checked against hand-computed expected exit codes.

Full test suite (183 tests across compile/error/opt) still passes, no warnings.

## Status

Phase 2 appears complete per the plan's own scope — nothing else was listed under it.
