# Your First Henceforth Program

Now that you wrote and ran a Hello World program, let's dive into something a bit more involved.


## Writing your first program

Create a new directory, or just use the one from the previous section. Then, create a file called `calculator.hfs`.

Add the following code to it:
```rs
fn add: (i32 i32) -> (i32) {}
```

Currently, this code won't run, but let's break down what's happening.
- `fn` declares a function
- `add` is the function's name
- After the colon, we have the function signature, in this case `(i32 i32) -> (i32)`. This function takes two 32 bit ints, and returns one 32 bit int. In Henceforth, arguments don't have names - they're accessed from the stack instead (More on this below).

Now, let's complete this function:
```rs
fn add: (i32 i32) -> (i32) {
  @(+);
}
```
- `@(...)` denotes a stack block. Everything inside these blocks manipulates the current function's stack.
- When a function is executed, its arguments are put into that function's stack by the order they were passed.
```rs
Stack: [5, 3] // after calling add with the arguments (5 3)
```
- `+` is a special stack operator that pops two numbers, adds them, and pushes the result to the function's stack.
**Stack state:**
```rs
[5, 3]  // after calling add
  ↓
[8]     // after +
```
- When a function returns, the function stack must have exactly what the function's return type says.

Now, to finish this, let's make our main function:
```rs
...
fn main: () -> () {
  @("Enter a number: ") &> print;
  &> input_int;
  @("Enter a number: ") &> print;
  &> input_int;
  &> add;
  &> print;
}
```
- We create our function as before, `main` takes no arguments and has no return type.
- We put the string `"Enter a number: "` in the stack and print it using the `print` function, which prints the top of the stack to the terminal.
- Then, we call `input_int`, a function that asks the user for an integer and places it in the stack.
- We repeat the two previous steps again for the second number.
- We then call `add`, moving the arguments with the `&>` operator.
- We call `print` again to print the final result.

Here's the complete program:
```rs
fn add: (i32 i32) -> (i32) {
  @(+);
}

fn main: () -> () {
  @("Enter a number: ") &> print;
  &> input_int;
  @("Enter a number: ") &> print;
  &> input_int;
  &> add;
  &> print;
}
```

## Running your first program


Now, run it:
```bash
$ henceforth calculator.hfs
Enter a number: 3
Enter a number: 4
7
```

Congratulations, you have now written a simple Henceforth program!

## What's Next

Try experimenting with the code:
- Create a `subtract` or `multiply` function
- Add more operations to the output

