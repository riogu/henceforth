# Variables

Variables are one of the many imperative features that make Henceforth so powerful. Instead of relying entirely on the stack, they allow for keeping state locally.

## Variable declaration

Variables can be declared with the following syntax:
```
let x: i32;
let y: (i32 string);
```
Variables can't be initialized on declaration, they **must** use values on the stack.
```
let x: i32 = 5; // not valid
```

## How variables interact with the stack

Variables can be assigned values directly from the stack:
- `:=` copies the top value from the stack into the variable
- `&=` moves the top value from the stack, popping it, into the variable
```
let var: i32;
@(1 2 3) := var; // stack: [1, 2, 3], var = 3
```
```
let var: i32;
@(1 2 3) &= var; // stack: [1, 2], var = 3
```

The same rules apply to tuples:
```
let var: (i32 i32);
@((1 2)) := var; // stack: [(1 2)], var = (1 2)
```
```
let var: (i32 i32);
@((1 2)) &= var; // stack: [], var = (1 2)
```

Variables can be pushed onto the stack like any other literal.
```
let var: bool;
@(5); // stack: [5]
@(6); // stack: [5 6]
@(>); // stack: [false]
:= var; // stack: [false], var = false
@(var); // stack: [false, false], var = false
```
## Scoping rules

Variables are always local to their scope, and you can't declare variables on a global scope.

```
let x: i32; // not valid

fn foo: () -> () {
  let x: i32; // valid
  @(x); // valid
  ...
}

fn main: () -> () {
  @(x); // invalid
  ...
  if @(...) {
    let y: i32; // valid
  }
  @(4) := y; // invalid
}
```
