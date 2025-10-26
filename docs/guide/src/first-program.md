# Your First Henceforth Program

Now that you wrote and ran a Hello World program, let's dive into something a bit more involved.

Create a new directory, or just use the one from the previous step. Then, create a file called `calculator.hfs`.

Add the following code to it:
```
fn add: (i32 i32) -> (i32) {
  return;
}
```

Currently, this code won't run, but let's break down what's happening.
- `fn` declares a function
- `add` is the function's name
- After the colon, we have the function signature, in this case `(i32 i32) -> (i32)`. This function takes two 32 bit ints, and returns one 32 bit int. In Henceforth, arguments don't have names - they're accessed from the stack instead (More on this below).
- Inside the function body, denoted by curly braces, `return` ends the function's execution. (Note: semicolon rules are covered in the Language Reference)

Now, let's complete this function:
```
fn add: (i32 i32) -> (i32) {
  @(+) return;
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
  @("5 + 3 = " ((5 3)add)show +);
  @((...)print ()pop_all) return;
}
```
- We create our function as before, `main` takes no arguments and has no return type.
- We add the string `"5 + 3 = "` to the stack, followed by calling `add` with arguments `5` and `3` using the syntax `(5 3)add` - in Henceforth, arguments come before the function name. We then pass that to `show`, which converts a number into a string.
**Stack state:**
```
["5 + 3 = ", "8"]
```
- Since `print` only takes one argument, we concatenate the strings with `+`, similarly to how we added numbers.
- We call `print` with `(...)print`. The `(...)` tells Henceforth to use whatever values are already on the stack (our concatenated string) as arguments, rather than pushing new values first.
- After that, since `main` doesn't return anything but might have leftover values on the stack, we use `pop_all` to clear the stack before returning. (Note: `()` means pop_all takes no arguments)
- At the end, we return from our main function, finishing the program's execution.

Here's the complete program:
```
fn add: (i32 i32) -> (i32) {
  @(+) return;
}

fn main: () -> () {
  @("5 + 3 = " ((5 3)add)show +);
  @((...)print ()pop_all) return;
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

