# Stack Operations

Being a stack-based language, the stack is the core of Henceforth. This data structure has a LIFO policy, where the last element that went in the stack is the first element to come out. 

## Key differences from other stack-based languages

- In Henceforth, every function has its own stack, and these stacks can be seamlessly merged.
- Since not every operation affects the stack, Henceforth explicitly states whether something is changing it.
- For convenience, the interpreter handles some of the stack manipulation when switching contexts.

## Stack blocks
Stack blocks are Henceforth's way of separating imperative from stack-based code. They are delimited by `@(` and `)`. They don't have to be terminated by semicolons.

## Pushing values to the stack

To push a value to the stack, just write a literal or a variable inside a stack block.
```
@(5) // pushes 5
@(5.0) // pushes 5.0
@(true) // pushes true
@((1 2 3)) // pushes (1 2 3)
@("string") // pushes "string"
@(var) // pushes the value of var, if var is a variable and has a value
```

## Stack Operators
The following operators are allowed on the stack:
```
+ // pops two numbers, pushes their sum OR pops two strings, pushes their concatenation
- // pops two numbers, pushes their difference
* // pops two numbers, pushes their product
/ // pops two numbers, pushes their quotient
% // pops two numbers, pushes their integer division remainder
== // pops two numbers, pushes true if they're equal and false otherwise
!= // pops two numbers, pushes true if they're not equal and false otherwise
< // pops two numbers, pushes true if the left value is less than the right value and false otherwise
> // pops two numbers, pushes true if the left value is greater than the right value and false otherwise
<= // pops two numbers, pushes true if the left value is less than or equal to the right value and false otherwise
>= // pops two numbers, pushes true if the left value is greater than or equal to the right value and false otherwise
! // pops a bool, pushes its inverse
|| // pops two bools, pushes true if any of them are true and false otherwise
&& // pops two bools, pushes true if both are true and false otherwise
```

## Stack manipulation builtins

There are several keywords for working with the stack. These are denoted by the `@` prefix and work with the stack despite not being in a stack block.
```
@pop // removes the argument from the stack
@pop_all // removes everything from the stack
@dup // duplicates its argument
@swap // swaps its arguments
@over // copies second element to top of stack
@rot // moves third element to the top of the stack
@rrot // moves first element to the third position of the stack (equivalent to calling rot twice)
@nip // pops second element from the stack
@tuck // copies first element to the third position of the stack
```

## Common mistakes

Some common mistakes when using the stack include:
```
@(+) // ERROR: stack underflow
```
A stack underflow occurs when an operator expects more values than there are on the stack.
```
@(5 "hello" +) // ERROR: type mismatch
```
A type mismatch occurs when an operators expects values of a certain type but is given other types.
