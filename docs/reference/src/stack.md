# Stack Operations

Being a stack-based language, the stack is the core of Henceforth. In case you're unfamiliar with this data structure, it has a last-in first-out policy, where the last element that went in the stack is the first element to come out. 

## Stack blocks
Stack blocks are Henceforth's way of separating imperative code from code that manipulates the stack directly. They are delimited by `@(` and `)`.

## Pushing values to the stack

To push a value to the stack, just write a literal or a variable inside a stack block.
```rust
@(5) // () -> (5)
@(5.0) // (5) -> (5 5.0)
@(true) // (5 5.0) -> (5 5.0 true)
@("string") // (5 5.0 true) -> (5 5.0 true "string")
@(var) // (5 5.0 true "string") -> (5 5.0 true "string" var)
       // var is only pushed to the stack if it has a value
```

## Stack operators
The following operators are allowed on the stack:
```rust
+ // (i32 i32) -> (i32) or (f32 f32) -> (f32) or (str str) -> (str)
- // (i32 i32) -> (i32) or (f32 f32) -> (f32)
* // (i32 i32) -> (i32) or (f32 f32) -> (f32)
/ // (i32 i32) -> (i32) or (f32 f32) -> (f32)
% // (i32 i32) -> (i32) or (f32 f32) -> (f32)
== // (i32 i32) -> (bool) or (f32 f32) -> (bool)
!= // (i32 i32) -> (bool) or (f32 f32) -> (bool)
< // (i32 i32) -> (bool) or (f32 f32) -> (bool)
> // (i32 i32) -> (bool) or (f32 f32) -> (bool)
<= // (i32 i32) -> (bool) or (f32 f32) -> (bool)
>= // (i32 i32) -> (bool) or (f32 f32) -> (bool)
! // (bool) -> (bool)
|| // (bool bool) -> (bool)
&& // (bool bool) -> (bool)
[] // ([N]T i32) -> (T)
```
> [!warning] 
> Out-of-bounds array access is checked and will cause a runtime panic. Read the [section about arrays](./arrays.md) for more information on this topic. 

## Stack keywords

There are several keywords for working with the stack. These are denoted by the `@` prefix and work with the stack despite not always being in a stack block.
```rust
@pop // (a) -> ()
@pop_all // removes everything from the stack
@dup // (a) -> (a a)
@swap // (a b) -> (b a)
@over // (a b) -> (a b a)
@rot // (a b c) -> (b c a)
@rrot // (a b c) -> (c a b) (equivalent to calling rot twice)
@nip // (a b) -> (b)
@tuck // (a b) -> (b a b)
@rev // reverses the entire stack
@depth // puts the current depth of the stack onto it, not including this element
```

## Key differences from other stack-based languages

- In Henceforth, every function has its own stack, and these stacks can be seamlessly merged when entering or exiting a function (more on this [here](./functions.md)).
- Since not every operation affects the stack, Henceforth explicitly states whether something is changing it.
- For convenience, the compiler handles some of the stack manipulation when switching contexts, such as when entering or returning from a function, or when entering the body of an if statement.

## Common mistakes

Some common mistakes when using the stack include:
```rust
@(+) // ERROR: stack underflow
```
If you attempt to use an operator or call a function without enough elements on the stack, you will get a stack underflow error. 

```rust
@(5 "hello" +) // ERROR: type mismatch
```
A type mismatch occurs when an operator or function expects values of certain types but is given other types.
