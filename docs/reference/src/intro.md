# Introduction

Henceforth is a stack-based programming language that emphasizes direct stack manipulation, in a more modern way than its predecessors, like Forth or Joy, while also giving users the convenience of imperative programming.
Unlike traditional languages with nested function calls and expressions, Henceforth operations work with a stack data structure, giving programmers fine-grained control over the program's execution, while also being user-friendly.

## Key Characteristics
### Stack-based execution
At the core of Henceforth is the stack. Instead of complex expressions being evaluated and stored into variables, Henceforth has a more step-by-step approach, and variables are nothing more than wrappers for simple values. This makes data processing more explicit, predictable and natural.

### Explicit stack manipulation
By integrating imperative programming features into a stack-based language, it might get a little confusing whether or not something affects the stack. Henceforth uses special notation to fix this issue, making it explicit when something is manipulating the stack.

### Seamless context switching
Unlike Forth, where everything is in one stack, Henceforth creates a new stack for every function, merging the callee and caller's stacks when the function is done executing, all done in a way that feels natural.
Control flow is also handled by the compiler, so that, for example, __every__ branch of an if statement starts with the same stack.

### Strong Static Typing
Despite its low-level feel, Henceforth is statically typed with types like `i32`, `f32`, `string`, and `bool`. The type system ensures stack operations are safe and well-defined.

## Quick Feature Overview
### Functions with Stack-Based Arguments
```
fn add: (i32 i32) -> (i32) {
  @(+) return;
}
```

### Stack Blocks for Direct Fine-grained Manipulation
```
@(1 3 + 2 *)  // Push 1, push 3, add, push 2, multiply
```

### Several Ways to Call Functions
```
@((5 3)add) // Explicit arguments
@(5 3 (...)add) // Implicit arguments (values already on stack)
@(5 (... 3)add) // Partially implicit arguments (first value already on stack)
```

