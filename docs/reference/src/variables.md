# Variables

Variables are one of the many imperative features that make Henceforth so powerful. Instead of relying entirely on the stack, they allow for keeping state locally.

## Declaring variables

Variables are declared with a name and a type:
```rust
let x: i32;
let y: [5]str;
```
> [!NOTE]
> Variables can't be initialized on declaration, they **must** be assigned from the stack afterward:
```rust
let x: i32 = 5; // not valid
```

## Assigning from the stack

Once declared, variables can be assigned a value from the top of the stack using:
- `:=` (copy)
- `&=` (move)
See [the section on assignments and calls](./assignments-and-calls.md) for more details on the distinction between the two.
```rust
let var: i32;
@(1 2 3) // () -> (1 2 3)
:= var; // (1 2 3) -> (1 2 3), var = 3
```
```rust
let var: i32;
@(1 2 3) // () -> (1 2 3)
&= var; // (1 2 3) -> (1 2), var = 3
```

## Reading variables

Variables can be pushed onto the stack like any other literal.
```rust
let var: bool;
@(5 6)  // () -> (5 6)
@(>) := var; // (5 6) -> (false), var = false
@(var) // (false) -> (false false)
```
> [!WARNING]
> Reading from an unassigned variable will cause an error.

## Scoping rules

Variables are always local to their scope, and you can't declare variables on a global scope.

```rust
let x: i32; // not valid

fn foo: () -> () {
  let x: i32; // valid
  @(x); // valid
}

fn main: () -> () {
  @(x); // invalid, x is not visible here
  if @(...) {
    let y: i32; // valid, scoped to this block
  }
  @(4) := y; // invalid, out of scope
}
```
