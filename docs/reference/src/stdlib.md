# Standard Library Functions
Henceforth contains many functions in the standard library that allow for more abstraction out-of-the-box.

## String Manipulation and IO Functions
Currently, the Henceforth standard library only contains three functions for string manipulation:
```
fn i32_to_str: (i32) -> str;
fn i32_to_str: (f32) -> str;
fn bool_to_str: (bool) -> str;
```
This converts an `i32`, `f32` or `bool` into a string.
A function like this goes hand-in-hand with IO, for which Henceforth contains two functions:
```
fn print: (str) -> (str);
fn print_stack: () -> ();
```
`print_stack` differs from other functions as it affects the caller's entire stack, without explicitly passing it, printing it in reverse order (bottom to top), and works with any type.
`print` simply prints its argument.

