# Control Flow

In other stack-based languages, control flow is handled within the stack, leading to some questionable syntax (_looking at you, Forth_). Henceforth fixes this by handling it imperatively, leading to a more familiar way of doing things.

## While loops
`while` loops are the primary way of repeating things in Henceforth, since `for` loops don't exist.
They are used in the following way:
```
while @(<condition>) {
  <body>
}
```
As expected, the body of the loop will be repeated while the condition is still true.
The one thing Henceforth does differently from other stack-based languages is that, when looping, it pops the result of evaluating the condition when entering the body, and when leaving the body after the condition is evaluated as `false`.
__In Henceforth, a while loop must not change the depth of the stack.__

This means that, if something changes the stack inside the loop's body, it must be stored in a variable before the next iteration.

The following keywords can be used inside `while` loops to change its execution:
- `break` - leaves the loop entirely
- `continue` - skips the rest of the body's execution, starting the next iteration

## If statements

`if` statements can be used to conditionally perform operations.
They are used in the following way:
```
if @(<condition1>) {
  <if-body>
} else if @(<condition2>) {
  <else-if-body>
} else if ... {
  ...
} else {
  <else-body>
}
```
This syntax mirrors imperative syntax from languages like C, instead of the confusing `<condition> if <body> then ...` of Forth.

Like in `while` statements, whenever a new branch is entered, the stack is restored to its previous state before the loop, and the result of evaluating the condition is always popped automatically.
In an `if` statement, all branches must have the same stack depth.

## Example

This all may seem very confusing, let's look at a simple FizzBuzz program.
```
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
- When entering the `if` statement the stack contains only the argument.
- The condition's stack block is executed, leaving `true` or `false` in the stack.
- Assuming the evaluated condition was true, when entering the body, this value is popped, so we add `"fizzbuzz"` to the stack and return with a stack that matches the expected return type.
- If the condition was false, we move on to the next branch. The stack is restored to just having the argument, and the condition is evaluated in the same way as before.
- When reaching the `else`, the stack only contains the argument, and since there is no condition, it stays there while entering the body, so we have to pop it.
- We then push the string `"no fizzbuzz"` and return as expected.

Essentially, control flow boils down to these rules:
- Every branch of an `if` statement and every iteration of a `while` loop, starts with the same exact stack
- All branches of an `if` statement must have the same final stack depth
- A `while` loop must maintain stack depth
- When entering the `else` branch of an `if` statement, since there's no condition, everything stays there
