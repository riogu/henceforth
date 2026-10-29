# Builtins
As of version 1.0.0, Henceforth does not contain a standard library. It does contain several builtin functions that are useful for I/O.

## I/O Functions

Henceforth contains the following functions for reading input from the user and printing it out:
```rs
fn print: (T) -> ();
fn print_stack: () -> ();
fn input_int: () -> (i32);
fn input_float: () -> (f32);
fn input_str: () -> (str);
```
`print_stack` differs from other functions as it affects the caller's entire stack, without explicitly passing it, printing it in reverse order (bottom to top), and works with any type.
`print` simply prints its argument, and works with any type.
The `input_*` functions all take input from the standard input, and put the return value in the stack. There are no internal checks to this input, e.g. passing a string as input when `input_int` is called is perfectly valid (will simply print 0).

