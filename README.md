# henceforth
https://riogu.github.io/henceforth
Example:
```rs
// Calculate factorial using stack operations
fn factorial: (i32) -> (i32) {
    let n: i32;
    let result: i32;
    
    &= n;  // pop stack and assign to n
    @(1) := result;  // copy top of stack to result
    
    while @(n 1 >) {
        @(result n *) &= result;  // pop and assign
        @(n 1 -) &= n;  // pop and assign
    }
    
    @(result) return;
}
// Helper to generate a range on the stack
fn push_range: (i32 i32) -> (i32 i32 i32 i32 i32) {
    let start: i32;
    let end: i32;
    
    @(123123)
    &= end;  // pop and assign
    &= start;  // pop and assign
    
    @(start)
    while @(dup end <) {
        @(1 + hfs::dup)  
                   
    }
    @(hfs::pop);
}

// Main program: calculate sum of factorials from 1 to 5
fn main: () -> (i32) {
    let sum: i32;
    let temp: i32;
    
    @(0)
    := sum;  // copy to sum
    @((1 5)push_range);
    
    // Stack now has: 1 2 3 4 5
    while @(hfs::depth 0 >) {
        &= temp;  // pop and assign
        @(temp) factorial;
        @(sum +) &= sum;  // pop and assign
    }
    
    @(sum) return;  // Returns 153 (1! + 2! + 3! + 4! + 5!)
}
```
