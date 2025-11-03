# Your First Henceforth Program

Now that you wrote and ran a Hello World program, let's dive into something a bit more involved.

Create a new directory, or just use the one from the previous step. Then, create a file called `calculator.hfs`.

Add the following code to it:
```
fn add: (i32 i32) -> (i32) {}
```

Currently, this code won't run, but let's break down what's happening.
- `fn` declares a function
- `add` is the function's name
- After the colon, we have the function signature, in this case `(i32 i32) -> (i32)`. This function takes two 32 bit ints, and returns one 32 bit int. In Henceforth, arguments don't have names - they're accessed from the stack instead (More on this below).

Now, let's complete this function:
```
fn add: (i32 i32) -> (i32) {
  @(+);
}
```
- `@(...)` denotes a stack block. Everything inside these blocks manipulates the current function's stack.
- When a function is executed, its arguments are put into that function's stack by the order they were passed.
```
Stack: [5, 3] // after calling add with the arguments (5 3)
```
- `+` is a special stack operator that pops two numbers, adds them, and pushes the result to the function's stack.
**Stack state:**
```
[5, 3]  // after calling add
  ↓
[8]     // after +
```
- When a function returns, the function stack must have exactly what the function's return type says.

Now, to finish this, let's make our main function:
```
...
fn main: () -> () {
  @("5 + 3 = " 5 3);
  &> add;
  &> i32_to_str;
  @(+);
  &> print;
  @pop_all;
}
```
- We create our function as before, `main` takes no arguments and has no return type.
- We add the string `"5 + 3 = "` to the stack, followed by `5` and `3`.
- Then, we call `add`, moving the arguments with the `&>` operator.
- We now call `i32_to_str`, which converts a number into a string, also moving the argument.
**Stack state:**
```
["5 + 3 = ", "8"]
```
- Since `print` only takes one argument, we concatenate the strings with `+`, similarly to how we added numbers.
- We call `print`, moving its arguments, then we use the `@pop_all` keyword to pop the string we just printed.

Here's the complete program:
```
fn add: (i32 i32) -> (i32) {
  @(+);
}

fn main: () -> () {
  @("5 + 3 = " 5 3);
  &> add;
  &> i32_to_str;
  @(+);
  &> print;
  @pop_all;
}
```

Now, run it with `henceforth calculator.hfs`. The expected output should be:
```bash
5 + 3 = 8
```
Congratulations, you have now written a simple Henceforth program!

## What's Next

Try experimenting with the code:
- Change the numbers in the calculation
- Create a `subtract` or `multiply` function
- Add more operations to the output

