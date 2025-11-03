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

There are two operators to call a function:
```
@(1 2 3) :> foo;
@(1 2 3) &> foo;
```

The `:>` operator copies the arguments, keeping them in the stack, and the `&>` operator moves the arguments, removing them from the stack. This syntax closely resembles the `:=` and `&=` operators, and their behavior is similar.

## How context switches between function calls

In Henceforth, functions have separate stacks, and these stacks are merged between function calls.
Here's an example of a function that mimics the functionality of the `@dup` keyword.
```
fn duplicate: (i32) -> (i32 i32) {
  let temp: i32;
  := temp;
  @(temp);
}

fn main: () -> () {
  @(5) &> duplicate;
  @pop_all;
}
```
Now we can trace the program's execution:
- When `main` (the entry point for any Henceforth program) starts running, the stack is empty.
- When calling `duplicate` with the argument `5`, the following happens:
  - `5` is pushed into the stack of `main`, then we call dup by moving its argument with `&>`. 
- When switching to the context of `duplicate`, the `main` stack is now empty, and the popped arguments are pushed into `duplicate`'s stack, which now contains `[5]`.
- The function then creates a variable, copies the top of the stack (`5`) into it, and pushes the variable on the stack, essentially duplicating the value. Now the stack of `duplicate` contains `[5, 5]`
- When finishing `duplicate`'s execution, `return` is not necessary, since the function will just return whatever is in the stack when the execution finishes. The `return` keyword is only necessary for early returns.
- When switching context, like before, we put it into the stack of `main`, so now it contains `[5, 5]`.
- If we tried returning now, the stack would be compared against the return type of main and it would fail. To fix that we use `@pop_all` to remove everything from the stack, so we can safely return.
