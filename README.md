# henceforth compiler/interpreter
Please check the henceforth github page for more info:
https://riogu.github.io/henceforth


# Example:
```rs
// Calculate factorial using stack operations
fn factorial: (i32) -> (i32) {
    let n: i32;
    let result: i32;
    
    &= n;  // pop stack and assign to n
    @(1) &= result;  // push 1 and assign to result
    
    while @(n 1 >) {
        @(result n *) &= result;  // result * n, pop and assign
        @(n 1 -) &= n;  // n - 1, pop and assign
    }
    
    @(result);  // push result to stack for return
}

// Helper to generate a range on the stack
fn push_range: (i32 i32) -> (i32 i32 i32 i32 i32) {
    let start: i32;
    let end: i32;
    
    &= end;  // pop and assign
    &= start;  // pop and assign
    
    @(start);  // push start
    while @(@dup end <) {  // duplicate top, compare with end
        @(1 +);  // add 1 to top of stack
        @dup;  // duplicate the result
    }
    @pop;  // remove the extra value (goes one past end)
}

// Main program: calculate sum of factorials from 1 to 5
fn main: () -> (i32) {
    let sum: i32;
    let temp: i32;
    
    @(0) &= sum;  // initialize sum to 0
    @(1 5) &> push_range;  // call push_range with 1 and 5
    
    // Stack now has: 1 2 3 4 5
    while @(@depth 0 >) {  // while stack has elements
        &= temp;  // pop and assign
        @(temp) &> factorial;  // call factorial
        @(sum +) &= sum;  // add to sum
    }
    
    @(sum);  // push sum for return
}
```
