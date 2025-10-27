# Types

Henceforth's simple type system with static typing separates it from other stack-based languages, allowing for a more user-friendly experience.

## Primitive Types
Henceforth has four basic types:
- `i32` - 32-bit signed integer
- `f32` - 32-bit floating-point
- `str` - UTF-8 encoded text
- `bool` - `true` or `false`

For grouping values, there are also tuples, denoted by parentheses around values. Note that the `(...)` notation for function calls creates a tuple and passes it to the function, however, `...` is not allowed in any other context.

## Type Annotations
Henceforth doesn't have type inference, and all types **must** be specified.
```
fn foo: (i32 str) -> (bool) { ... }
         ^^^^^^^      ^^^^
         parameters   return type
```

## Type Conversions
There is only one standard library function to convert types:
```
fn show: (i32) -> str {...}  
fn show: (f32) -> str {...}  
```

Operators implicitly convert between `i32` and `f32`.

## Type Compatibility
Being statically typed, Henceforth validates stack operations and function calls before running the program.
