# Control Flow

In other stack-based languages, control flow is handled within the stack, leading to some questionable syntax (_looking at you, Forth_). Henceforth fixes this by handling it imperatively, leading to a more familiar way of doing things.

## While loops
`while` is the only looping construct in Henceforth (there are no `for` loops).
They are used in the following way:
```rs
while @(<condition>) <optional function calls> {
  <body>
}
```

The condition is a stack block, optionally followed by function calls (e.g. `while @(x) &> is_positive {`), and it must leave a `bool` on top of the stack. The body repeats while that `bool` is `true`. Unlike in Forth, the `bool` is popped automatically, so the body never sees it.


> [!WARNING]
> A `while` loop must not change the stack. At the end of every iteration, the stack must have the same depth and types as it had at the start, since the next iteration begins from there.

In practice, anything the body produces has to be consumed before the next iteration, usually by moving it into a variable:


```rs
fn less_than_ten: (i32) -> (bool) { @(10 <) }

fn sum_below_ten: () -> (i32) {
  let i: i32; @(0) &= i;
  let sum: i32; @(0) &= sum;
  while @(i) &> less_than_ten {
    @(sum i +) &= sum; // the result is moved into `sum`, so the stack is unchanged
    @(i 1 +) &= i;
  }
  @(sum);
}
```

The following keywords can be used inside `while` loops to change its execution:
- `break` - leaves the loop entirely
- `continue` - skips the rest of the body's execution, starting the next iteration

## If statements

`if` statements can be used to conditionally perform operations.
They are used in the following way:
```rs
if @(<condition1>) <optional function calls> {
  <if-body>
} else if @(<condition2>) <optional function calls> {
  <else-if-body>
} else if ... {
  ...
} else {
  <else-body>
}
```


As in `while`, each condition is a stack block, optionally followed by function calls, that leaves a `bool`, which is popped automatically.

Since conditions consume their operands, what the stack looks like when a body starts matters:
- Every condition is evaluated starting from the stack as it was at the beginning of the `if` statement. If the condition is `false`, the stack is restored to that state and the next condition is tried.
- If a condition is `true`, its `bool` is popped and the body runs on whatever the condition left behind.
- The `else` branch has no condition, so it runs on the stack as it was at the beginning of the `if` statement.
- **All branches must end with the same stack** (same depth and types).

> [!NOTE]
> An `if` without an `else` must leave the stack as it was at the beginning of the statement, since the "nothing happened" path has to match.

## Example

This all may seem very confusing, let's look at a simple FizzBuzz program.
```rs
fn fizz_buzz: (i32) -> (str) {
	if @(15 % 0 ==) {
		@("fizzbuzz");
	} else if @(3 % 0 ==) {
		@("fizz");
	} else if @(5 % 0 ==) {
		@("buzz");
	} else {
		@pop;
		@("no fizzbuzz");
	}
}
```

Let's trace the program's execution:
- On entering the `if` statement, the stack contains only the argument: `(n)`.
- The first condition runs. `15 %` consumes `n`, so the stack ends up as just `(bool)`.
- If the condition was `true`, the `bool` is popped, leaving `()`. The body pushes `"fizzbuzz"`, so the stack is `(str)`, which matches the return type.
- If it was `false`, the stack is restored to `(n)` and the next condition is evaluated the same way.
- Reaching the `else`, there is no condition, so the stack is still `(n)`. This is why only the `else` needs a `@pop`: the conditions consumed the argument, but the `else` never ran one. We pop `n`, push `"no fizzbuzz"`, and end with `(str)` as expected.

## Summary

- Every `if` condition starts from the stack at the beginning of the statement, and a `false` condition restores it.
- A `true` branch starts from what its condition left, minus the `bool`. The `else` starts from the stack at the beginning of the statement.
- All branches of an `if` statement must end with the same stack.
- A `while` loop must end every iteration with the stack it started with.
