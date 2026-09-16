# Contributing to Henceforth

The Henceforth repository contains all the code necessary to the development of Henceforth. Contributions of any form are welcome.

Like any other open-source project, contributing to Henceforth requires an up-to-date build.

If you have any questions beyond the scope of this document, feel free to ask @riogu or @joao-novo.

## Bug Reports

If you found a bug in Henceforth:
- Search the existing issues for duplicate bug reports. Make sure to check the closed issues, since regressions can and will most likely happen.
- For bugs, add a reproducible test case, preferably as minimal as possible, and a backtrace in case of ICE (use `RUST_BACKTRACE=1`), or the compiler output in case of an unexpected pass or fail.

## Suggesting a Feature

If you want to suggest a new feature:
- Search the existing issues to see if the feature has already been suggested.
- If not, open a new issue describing the feature.
- Provide details on how it should be implemented, and add sources to back up your suggestion.
- Discuss the feature with the maintainers.

## Submitting a Patch

When submitting a patch, there's a few things you should keep in mind:
- As usual, make your commits in a fork of the repository, and submit a pull request.
- Use your best judgement for commit messages, there are no strict rules being enforced
- If your patch fixes a bug or adds a new feature, you are expected to add new test cases covering the new behavior added.
  As an exception, if your patch corrects already existing failing tests, you should mention that in the PR.
- If your patch adds a new feature, it should also include documentation.
- All tests are expected to pass both locally and in the CI pipeline. Exceptionally, if the main branch contains failing tests in the CI, those don't have to pass the CI for your PR.
- Wait for your patch to reviewed by the maintainers. If it's been waiting for a while, ping @riogu or @joao-novo. 

## Repository Structure

The repository is currently structured in the following way (unimportant files and directories omitted):
```
├── docs/ // Documentation
│   ├── guide/
│   │   └── src/ // Getting Started guide
│   └── reference/
│       └── src/ // Language Reference
├── src/
│   ├── hfs/ // Main source code
│   │   ├── diagnostics/ // Error framework source code
│   │   └── ... // Main compiler source code
│   ├── hfscheck/ // Testing framework source code
│   └── main.rs // CLI entrypoint
└── tests/
    ├── compile_tests/ // Tests that are expected to compile
    ├── failure_tests/ // Tests that are expected to fail
    │   ├── lexer/
    │   ├── parser/
    │   └── stack_analyzer/
    └── opt_tests/ // Optimization tests
```

## Testing Framework

As mentioned before, any patch that adds new behavior needs to have test coverage.

To run the test suite, use the following command:
```
$ cargo t
```
This is an alias for `cargo test --no-fail-fast`, which guarantees that the entire test suite runs even if one harness fails.
More information on the flags and arguments that can be passed to this command can be found in the [cargo documentation](https://doc.rust-lang.org/cargo/commands/cargo-test.html).

Henceforth has two kinds of tests, inline tests, and `hfscheck` tests.

### Inline Tests

> [!WARNING]
> Inline tests are only meant to be used when contributing to `hfscheck`.
> PRs with inline tests outside of the `hfscheck` folder will not be accepted!

These kinds of tests are very straight-forward for everyone familiar with Rust tests.
There are no strict guidelines on how to write these, other than matching the existing tests' structure.

### `hfscheck` Tests

`hfscheck` is a custom test framework similar to LLVM's FileCheck or GCC's DejaGNU.
To write a test, create a new `.hfs` or `.hfsir` in the appropriate folder, write a regular Henceforth or HFS IR program, and add `hfscheck` directives.
When running the test suite, if it finds a file with directives in the right folder, new tests will be created automatically.

`hfscheck` directives are written in special single-line comments using `//?`, and use a custom simplified regex with the following syntax:

| Syntax | Meaning |
| --- | --- |
| . | Matches a single character |
| * | Matches 0 or more characters |
| {a, b, c} | Matches `a`, `b`, or `c` |
| [a-c] | Matches anything in the range between `a` and `c` |

Similarly to FileCheck, any usage of regular expressions must be wrapped in `{{...}}`. Keep in mind you can chain multiple regex elements in a single block, e.g. `{{{0, 1, 2}[A-Z]}}` matches a number from 0 to 2 and then a letter from A to Z.

Additionally, you can bind a regular expression to a variable with the following syntax with `[[X:.]]`, and then use that binding with `[[X]]`. This binds 'X' to what `.` _matched_ at that point, not to the pattern `.` itself. Later uses of `[[X]]` must match that exact captured value.
For example, `[[X:{a, b, c}]] [[X]]`, when matched against the input `a b` will bind the variable `X` with the value of the first match, `a`, and will fail when it attempts to match `[[X]]` against `b`. Even though `b` was one of the possible matches in the original binding, it captured `a`, and therefore cannot match `b`.

There are three kinds of `hfscheck` tests:

#### Compile Tests

If you want to assert that a given test case compiles cleanly, write a `.hfs` program, use the `COMPILE` directive, and place your test case in the `compile_tests/` directory.

This directive does not have any flags, and is the only directive allowed in files inside `compile_tests/`
For example:
```rust
//? COMPILE
fn main: () -> () {}
```
#### Failure Tests

If you want to assert that a test case throws an error, write the test case in a `.hfs`file, use the `ERROR` directive, and place it in the correct subfolder inside `failure_tests/`.
The subfolder is important as that will determine how far into the pipeline it will run.
Alongside the `ERROR` keyword, you can optionally provide:
- An error message in double quotes. Omitting this argument will match any error message. This error message uses the regex shown above.
- A location relative to the current line. Omitting this argument will match only if the thrown error is on the same line as the directive. This argument uses the syntax `@[<relative_position><n>]`, where:
  - `<relative_position>` is either `^`, `v`, or `.`. `^` restricts the match only to lines above the current line, `v` restricts it only to lines below the current line, and `.` restricts it only to the current line, ignoring `<n>` entirely. 
  - `n` is any number or `*`. This tells `hfscheck` that the error is exactly `n` lines above or below the current line, depending on `<relative_position>`. `*` will match any line above or below the current line. 
For example:
```rust
«
//? ERROR "unexpected character" @[^1]
```

#### Optimization Tests

If you want to test a middle-end optimization, write a `.hfsir` file, and place it in the `opt_tests/` folder.

To assert that your optimization/pipeline generates the correct code, use the `CHECK` directive.
`CHECK` directives come in blocks initiated by a special `CHECK FN "<fn_name>"` directive. This asserts the existence of this function and resets the pointer to the first line of the function. Any `CHECK`'s that follow it will only match lines below the pointer, and will move the pointer forward to the matched line, unless you reset the pointer to an earlier line with `CHECK FN`.
The kinds of `CHECK`'s allowed can be found in the table below, alongside their meaning:
| Check Kind | Meaning |
| --- | --- |
| `CHECK "<ir>"` | Asserts the existence of `<ir>` in the optimized code |
| `CHECK NOT "<ir>"` | Asserts the absence of `<ir>` in the optimized code. It does not advance the pointer. |
| `CHECK NEXT "<ir>"` | Asserts the existence of `<ir>` in the optimized code ONLY in the next line |
| `CHECK SAME "<ir>"` | Asserts the existence of `<ir>` in the optimized code ONLY in the same line as the previous check |
| `CHECK BLOCK "<block>"` | Asserts the existence of a block named `<block>` in the optimized code |
| `CHECK COUNT <n> "<ir>"` | Asserts the existence of exactly `<n>` occurrences of `<ir>` in the optimized code. Pointer moves to the last matching line. |

To tell `hfscheck` to use a specific pipeline, or to create your own custom pipeline, use the `OPT` directive.
Multiple `OPT` directives can be used in a file, and all of them will run sequentially. They do not overwrite each other.
Omitting this directive entirely will not run any optimizations.
Alongside the `OPT` keyword, you can provide several flags and arguments:
- `-O<n>`, where `n` can be 0, 1, 2, or 3. This will use the standard `O<n>` pipelines.
- A list of comma-separated optimizations inside square brackets, e.g. `[DCE, Mem2Reg]`. This creates a custom pipeline and will run the optimizations specified.
- `-iterative`. This tells `hfscheck` to run the pipeline repeatedly until it can't be optimized further.
- `-dump`. This will print the unoptimized and optimized code whenever this test is ran.

For example:
```llvm
//? OPT -O0 -iterative
fn fizz_buzz: (i32) -> (str) {
  start_2:
    %2 = i32 %arg0
    %3 = i32 15
    %4 = i32 %2 % %3
    %5 = i32 0
    %6 = bool %4 == %5
    branch %6, if_body_1 else_if_cond_1
  else_if_cond_1:
    %10 = i32 3
    %11 = i32 %2 % %10
    %12 = i32 0
    %13 = bool %11 == %12
    branch %13, if_body_2 else_if_cond_2
  if_body_1:
    %14 = str "fizz"
    jump if_end_1
  else_if_cond_2:
    %15 = i32 5
    %16 = i32 %2 % %15
    %17 = i32 0
    %18 = bool %16 == %17
    branch %18, if_body_3 else_body_1
  if_body_2:
    %19 = str "buzz"
    jump if_end_1
  else_body_1:
    %20 = str "no fizzbuzz"
    jump if_end_1
  if_body_3:
    %7 = str "fizzbuzz"
    jump if_end_1
  if_end_1:
    %8 = str phi [if_body_1: %7], [if_body_2: %14], [if_body_3: %19], [else_body_1: %20]
    %9 = (str) (%8)
    return %9
}

//? CHECK FN "fizz_buzz"
//? CHECK BLOCK "start_2"
//? CHECK NOT "alloca"
  
```

> [!NOTE]
> Using both `-O<n>` and a custom pipeline in the same `OPT` will always pick the `O<n>` pipeline.

## Documentation Guidelines

The Henceforth documentation is separated into two sections, the Getting Started guide, and the Language Reference.
The first one is a small tutorial with installation instructions and a small onboarding example, while the language reference contains a more exhaustive explanation of each feature. They are both user-facing, and should not be very technical, even if the feature implemented is complex.

Most PRs should not have to add to the Getting Started guide, unless justified. If you think your feature should add new documentation to it, clarify with the maintainers before submitting your PR.

Internally, the Henceforth docs use one `mdBook` for each section, and are built separately. To preview locally, from the root of the section you want to preview (`guide/` or `reference/`), run the following command:
```
$ mdbook serve
```
This creates a live server that will automatically reload when new changes are made.

For information on how `mdBook` works, check out [their own documentation](https://rust-lang.github.io/mdBook/).

When it comes to style and tone:
- Remember that this is user-facing, so keep the language tutorial-friendly and easily approachable.
- Make sure to provide guided examples on how your feature works.
- Do not explain how it was implemented.

Make sure to look at the existing documentation for examples on how to structure your own.

You do not need to build the documentation. When your PR gets merged, both `mdBook`s will be built and combined.

