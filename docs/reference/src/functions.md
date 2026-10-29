# Functions

Functions in Henceforth are statically typed, giving the programmer a better abstraction than the normal approach of "pop arguments from stack, do operation _x_, push return values to stack", ensuring richer behavior than other untyped stack-based languages.

## Declaring functions

Functions are declared with a  name, argument types, and return types:
```rs
fn function_name: (i32) -> (str bool) {
  ...
}
```
> [!NOTE]
> Functions must be declared on the global scope. Nested functions are not allowed.

## Calling functions

There are two operators to call a function:
```rs
@(1 2 3) // () -> (1 2 3)
:> foo; // (1 2 3) -> (1 2 3 <return values of foo>)
@pop_all;
@(1 2 3) // () -> (1 2 3)
&> foo; // (1 2 3) -> (<return values of foo>)
@pop_all;
```

This mirrors the `:=` and `&=` variable assignment operators, and behaves similarly. As with variable assignments, which are type-checked against the variable's declared type, a function call's arguments are type-checked against the function's declared parameter types.
More on the semantics between these operators on [Assignments and Calls](./assignments-and-calls).

## Context switching between calls

Every function has its own stack. When a function is called, the arguments are moved into the callee's stack. When it returns, its remaining stack is first type-checked against the declared return type, and then merged back into the caller's stack.

Here's an example of a function that mimics the `@dup` keyword, duplicating the top of the stack.
```rs
fn dup: (i32) -> (i32 i32) {
  let temp: i32;
  := temp; // (<arg>) -> (<arg>), temp = <arg>
  @(temp); // (<arg>) -> (<arg> <arg>)
}

fn main: () -> () {
  @(5) &> dup;
  @pop_all;
}
```
Now we can trace the program's execution:
- `main` starts with an empty stack.
- `@(5) &> dup` pushes 5, then moves it out of `main`'s stack and into `dup`'s stack. `main`'s stack is now `()`, `dup`'s is `(5)`.
- Inside `dup`, `:=` temp copies the top of the stack into `temp`, which now has the value `5`, without popping it from the stack.
- `@(temp)` pushes the value of that variable to the stack, so `dup`'s stack is now `(5 5)`.
- `dup` then returns. No explicit `return` statement is needed, since a function implicitly returns whatever is left on its stack.`return` is only required for early returns. As mentioned earlier, when leaving a function, before merging the stacks, its stack is type-checked against its return types. Since `dup` has return type `(i32 i32)` and the current stack is `(5 5)`, it matches the expected types and return successfully.
- On return, `dup`'s stack merges back into `main`'s, which is now `(5 5)`. Since we moved the argument of `dup` out of `main`'s stack when calling `dup`, `main`'s stack was left empty, so it is simply the return values of `dup`.
- To ensure that `main` type-checks on return, since this function is expected to have nothing on the stack on return, we pop all of the existing elements using `@pop_all`.
