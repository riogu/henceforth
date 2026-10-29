# Assignments and Calls

One of Henceforth's defining features is the duality between copy and move operators. Essentially, a copy operator will leave its operands on the stack, while a move operator consumes them.

Henceforth has the following copy and move operators:

| | Copy (keeps values on the stack) | Move (consumes them) |
|---|---|---|
| Variable assignment | `:=` | `&=` |
| Function call | `:>` | `&>` |
| Array assignment | `[:]=` | `[&]=` |

Copy operators have `:`, while move operators have `&`.

## Variable assignment operators

The variable assignment operators are quite straightforward:
```rs
fn main: () -> () {
  let x: i32;
  @(1)
  := x; // (1) -> (1), x = 1; the value assigned to x stays in the stack
  @pop;
  @(3);
  &= x; // (3) -> (), x = 3; the value assigned to x is removed from the stack
}
```

In variable assignments, doing a copy assignment is essentially equivalent to duplicating the value before doing a move assignment.

## Function call operators

Similarly to variable assignment operators, function calls behave as follows:
```rs
fn drop: (i32) -> () { @pop; }

fn add: (i32 i32) -> (i32) { @(+); }

fn no_op: () -> () {}

fn main: () -> () {
  @(1)
  :> drop; // (1) -> (1), in copy calls, even though the function returns nothing, the argument stays on the caller's stack
  @pop;
  @(1)
  &> drop; // (1) -> (), in move calls, the argument is consumed from the caller's stack

  // this also works with multiple argument functions
  @(1 2)
  :> add; // (1 2) -> (1 2 3), in copy calls, all arguments get copied into the callee's stack
  @pop_all;
  @(1 2)
  &> add; // (1 2) -> (3), in move calls, all arguments get moved to the callee's stack
  @pop_all;

  // calling a function that has no arguments with either operator will have the same exact result
  &> no_op;
  :> no_op;
}
```
> [!NOTE]
> From the callee's perspective, there is no difference between move and copy calls.

## Array assignment operators

The semantics for array assignment operators can essentially be seen as a combination of both function calls and variable assignments.
```rs
fn main: () -> () {
  let arr: [5]i32;
  @(1 0)
  [:]= arr; // (1 0) -> (1 0), arr = [1, _, _, _, _], both the index and the value stay in the stack in a copy array assignment
  @pop_all;
  @(2 1)
  [&]= arr; // (2 1) -> (), arr = [1, 2, _, _, _], both the index and the value are moved from the stack in a move assignment
}
```

## What actually gets copied and moved

Unlike languages with references like C++, Henceforth's copy and move semantics are much simpler. Arrays are values, and copying or moving one into a variable, function, or the stack produces an independent array, and writes never propagate back to the source. For example:
```rs
fn f: ([]i32) -> () { ... }

fn g: () -> ([]i32) { ... }

fn main: () -> () {
  let arr: [3]i32;
  @([1 2 3]) := arr;  
  @(9 0) [&]= arr; // this modifies `arr`, but keeps the array in the stack intact
  @pop_all;

  @(arr) &> f; // similarly, if `f` modifies the argument, this does not modify `arr`, the same happens with `:>`

  &> g;
  := arr;
  @(1 0) [&]= arr; // this does not modify the array on the stack, even though its length is unknown
  @pop_all;
}
```

## When to use which

Copy operators are useful when you need to use the value for something else, e.g. comparing a number with its factorial, zeroing out an array. Move operators are useful for data transformation, and will likely be more intuitive for the average user.
