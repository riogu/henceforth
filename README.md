# Henceforth Programming Language

A statically-typed stack-based programming language with explicit stack manipulation, implemented in Rust.

---

## Quick Overview

Henceforth is a stack-based programming language featuring:
- Explicit stack manipulation with dedicated syntax
- Static typing with compile-time type checking
- Stack-based function signatures and control flow
- Copy and move semantics for stack operations
- Interpreted execution with planned compilation support (LLVM backend)

For detailed information, see [the github page](https://riogu.github.io/henceforth) for language specification and usage documentation.

---

## Example Program

```rust
// Calculate factorial using stack operations
fn factorial: (i32) -> (i32) {
    let n: i32;
    let result: i32;

    &= n;                    // pop stack and assign to n
    @(1) &= result;          // push 1 and assign to result

    while @(n 1 >) {         // push n, push 1, compare
        @(result n *) &= result;  // result * n, pop and assign
        @(n 1 -) &= n;            // n - 1, pop and assign
    }

    @(result);               // push result to stack for return
}

// FizzBuzz implementation
fn fizz_buzz: (i32) -> (str) {
    if @(15 % 0 ==) {
        @("fizzbuzz");
    } else if @(3 % 0 ==) {
        @("fizz");
    } else if @(5 % 0 ==) {
        @("buzz");
    } else {
        @pop;                // remove unused value
        @("no fizzbuzz");
    }
}

// Main program: calculate and test factorial of 5
fn main: () -> () {
    @(5) &> factorial;       // call factorial with 5
    @(120 ==);              // compare result with 120

    @(15) :> fizz_buzz;     // call fizz_buzz (copy argument)
    @pop;                   // discard result
}
```

---

## Language Features

### Stack-Based Execution
- **Explicit Stack Blocks:** `@(1 2 +)` pushes 1, pushes 2, then adds them
- **Stack Manipulation:** `@dup` duplicates top value, `@pop` removes top value
- **Stack Introspection:** `@depth` checks current stack depth

### Type System
- **Primitive Types:** `i32`, `f32`, `bool`, `str`
- **Tuple Types:** Function parameters and return values are tuples
- **Static Type Checking:** Types validated during stack analysis pass

### Control Flow
- **Conditional Statements:** `if`/`else if`/`else` with stack-based conditions
- **Loops:** `while` loops with stack balance enforcement
- **Branch Validation:** All branches must leave stack in consistent state

### Memory Semantics
- **Copy Operations:** `:=` for assignment, `:>` for function calls (copies values)
- **Move Operations:** `&=` for assignment, `&>` for function calls (moves values)
- **Explicit Semantics:** No implicit copying or moving of stack values

---

## Implementation

### Frontend
- Hand-written lexer with token support for stack operations
- Hand-written recursive descent parser generating unresolved AST
- Two-pass stack analyzer for identifier resolution and type checking
- Scope resolution with stack depth validation

### Execution
- Tree-walking interpreter with runtime stack management
- LLVM IR backend (planned)

### Compiler Pipeline
```
Source → Lexer → Parser/Unresolved AST → Stack Analyzer/Resolved AST → CFG → (planned) MIR -> LLVM IR → Compilation
                                                                             ↓
                                                                             Interpreter
```

---

## Currently Working On

### Control Flow Graph (CFG) Analysis
Stack depth and type validation at control flow boundaries. Ensuring proper stack balance across function exits, early returns, and loop bodies.

**Current Status:** Basic CFG analyzer structure exists. Currently implementing:
- Stack validation at every function exit point
- Type checking at block boundaries
- Integration with existing stack analyzer

### Stack Keyword System
Built-in stack manipulation operations (`@dup`, `@pop`, `@depth`, `@swap`).

**Current Status:** Partial implementation. Lexer -> Parser -> Stack Analyzer support complete.

---

## Planned Features

### Compilation
- MIR (Mid-level IR) generation from resolved AST
- LLVM backend for native code generation
- Optimization passes at MIR level

### Type System Extensions
- User-defined struct types
- Generic types with stack-aware semantics
- Type inference for variable declarations

### Stack Operations
- Complete stack keyword system (`@swap`, `@rot`, `@over`, etc.)
- Stack frame introspection during debugging
- Compile-time stack depth checking improvements

### Standard Library
- I/O operations (currently no built-in printing)
- String manipulation beyond literals
- Collection types with stack-based iteration

---

## Project Goals

This language explores stack-based programming in a modern, statically-typed context. Unlike traditional stack languages (ex: Forth), Henceforth provides:
- Explicit stack manipulation syntax
- Function-scoped stacks with controlled merging
- Static type checking before execution
- Familiar control flow structures

The goal is to demonstrate that stack-based programming can be both explicit and ergonomic, serving as a foundation for exploring:
- Alternative evaluation models
- Explicit data flow in programs
- Compile-time stack analysis techniques

---

## Documentation

Documentation is available in the [the github page](https://riogu.github.io/henceforth)

---

## Project Structure

```
src/hfs/
├── lexer.rs              # Tokenization
├── parser.rs             # Unresolved AST generation
├── unresolved_ast.rs     # First-pass AST representation
├── stack_analyzer.rs     # Stack analysis and AST resolution
├── ast.rs                # Resolved AST representation
├── cfg_analyzer.rs       # Control flow graph analysis (planned)
├── hfs_mir.rs            # Mid-level IR (planned)
├── interpreter.rs        # AST interpreter
├── types.rs              # Type checking and type operations
├── token.rs              # Token definitions
├── scope_stack.rs        # Symbol resolution
└── builder/              # Test builders
```
