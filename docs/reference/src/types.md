# Types

Henceforth uses a simple, statically typed type system, which separates it from other stack-based languages, which tend to be dynamically typed or fully untyped. This allows for a more user-friendly experience.

## Henceforth type system
Henceforth has four primitive types:
- `i32` - 32-bit signed integer
- `f32` - 32-bit floating-point
- `str` - UTF-8 encoded text
- `bool` - `true` or `false`

There are also arrays, represented by `[<n>]<type>` where `<type>` is any primitive type or an array, and n is an expression representing the size of an array. Unsized array types are allowed in function parameters and return types. More on arrays [here](./arrays.md).


## Type annotations
Henceforth doesn't have type inference, and all types **must** be specified in both function and variable declarations.
```
fn foo: ([][]i32 str) -> (bool str) { ... }
         ^^^^^^^^^^^      ^^^^^^^^
         parameters       return types
```
> [!note]
> Multiple parameters and return types are space-separated. For more on this, see [the section about functions](./functions.md).

## Type conversions
As of now, Henceforth does not have any explicit way to convert between types. The only implicit type conversions happen when arrays lose their size under certain conditions, explained in their [own section](./arrays.md).

## Type compatibility

Henceforth checks types at compile time (if a program has a type error, it will not compile, or if using the interpreter, it will not run).

Two values are type-compatible only if they have the exact same type, with the exceptions described in [Type Conversions](#type-conversions).

Type checking applies to stack operations (except for [stack keywords](./stack.md#stack-keywords), which only check the current stack depth), variable assignment, array assignment and function calls. The compiler verifies that the values on the stack match what is expected.
