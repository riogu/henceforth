# Standard Library Functions
Henceforth contains many functions in the standard library that allow for more abstraction out-of-the-box.

## String Manipulation and IO Functions
Currently, the Henceforth standard library only contains one function for string manipulation:
```
fn show: (i32) -> str;
fn show: (f32) -> str;
fn show: (bool) -> str;
```
This converts any `i32`, `f32` or `bool` into a string.
A function like this goes hand-in-hand with IO, for which Henceforth contains two functions:
```
fn print: (str) -> (str);
fn print_stack: () -> ();
```
`print_stack` differs from other functions as it affects the caller's entire stack, without explicitly passing it, printing it in reverse order (bottom to top), and works with any type.

`print` simply prints its argument.

## Introspection
There is only one introspection function in the standard library:
```
fn typeof: (i32) -> (str);
fn typeof: (f32) -> (str);
fn typeof: (bool) -> (str);
fn typeof: (str) -> (str);
```
This function takes any value and returns its type as a string, which can be useful in control flow.

## Stack Operations
Stack operations make up most of the standard library. These functions can take any argument type, so we are annotating them with `T`, `U`, and `V`, even though generics don't exist in Henceforth:
```
fn pop: (T) -> (); // removes the argument from the stack
fn pop_all: () -> (); // removes everything from the stack
fn dup: (T) -> (T T); // duplicates its argument
fn swap: (T U) -> (U T); // swaps its arguments
fn over: (T U) -> (T U T); // copies second element to top of stack
fn rot: (T U V) -> (U V T); // moves third element to the top of the stack
fn rrot: (T U V) -> (V T U); // moves first element to the third position of the stack (equivalent to calling rot twice)
fn nip: (T U) -> (U); // pops second element from the stack
fn tuck: (T U) -> (U T U); // copies first element to the third position of the stack
```
Note that `pop_all` works similarly to `print_stack` where nothing is passed, yet something happens as a side effect.

