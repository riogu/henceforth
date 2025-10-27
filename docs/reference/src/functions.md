# Functions

Unlike other stack-based languages, functions in Henceforth are statically typed, giving a better abstraction than the normal approach of "pop from stack, push to stack".

## Declaring functions

Functions can be declared with the following syntax:
```
fn function_name: (i32) -> (str bool) {
  ...
}
/*
fn <name>: (<argument types>) -> (<return types>) {
  <body>
}
*/
```
They must be declared on a global scope.

## How function calls affect the stack

There are three ways a function can be called:
```
@((1 2 3)foo); // explicit arguments
@((...)foo); // implicit arguments, if arguments are already on the stack before the function call
@((... 2 3)foo); // partially implicit arguments, if some of the arguments are already on the stack
```

Function calls are seamlessly integrated with the type system, as passing arguments has the same syntax as creating a tuple. This means that all functions take a single tuple as an argument, and that tuple is popped and destructured when entering the function's context.

The `...` syntax is a special tuple operator that only works with function calls. It essentially completes the function signature with values from the stack.

## How context switches between function calls

In Henceforth, functions have separate stacks, and these stacks are merged between function calls.
Here's an example using the standard library function `dup` to illustrate this:
```
fn dup: (i32) -> (i32 i32) {
  let temp: i32;
  := temp;
  @(temp) return;
}

fn main: () -> () {
  @((5)dup);
  @(()pop_all) return;
}
```
Now we can trace the program's execution:
- When `main` (the entry point for any Henceforth program) starts running, the stack is empty.
- When calling `dup` with the argument `5`, the following happens:
  - `5` is pushed into the stack of `main`
  - A tuple is created with the previous value. Now the stack contains `[(5)]`
  - The function `dup` is pushed, and since it's a function, immediately pops along with its arguments, signaling the start of `dup`'s execution
- When switching to the context of `dup`, the `main` stack is now empty, and the popped arguments are destructured and pushed into `dup`'s stack, which now contains `[5]` (note the lack of tuple)
- The function then creates a variable, copies the top of the stack (`5`) into it, and pushes the variable on the stack, essentially duplicating the value. Now the stack of `dup` contains `[5, 5]`
- When calling return, a tuple is implicitly created with the entire stack of `dup`, and is compared against the expected return type (also a tuple). The stack of `dup`, before returning, contains `[(5 5)]`. This matches the expected return type, so we can switch back to the previous context of `main`.
- When switching context, like before, we destructure the tuple and put it into the stack of `main`, so now it contains `[5, 5]`.
- If we tried returning now, the stack would be compared against the return type of main (an empty tuple) and it would fail. To fix that we use `pop_all` to remove everything from the stack, so we can safely return (note that when we implicitly create a tuple and the stack is empty, the tuple created will be empty).
