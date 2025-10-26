## Code examples
```cpp
import hfs;

fn func: (i32 i32 f32) -> (str i32 i32 i32) {
    // then, here func pushes a tuple.
}

fn main: () -> (i32) {
    @(1 2 3);
    @((...)func);
    // i propose that (...) does the following:
    // - checks the type of whatever comes after it
    // - uses that information [in this case, (i32 i32 i23)] to turn the top of the stack
    // into a tuple. that means that:
    @(1 2 3 (...)); // is valid.
    // after this, the stack is:
    @((1 2 3)); // one tuple element
    // this also means that:
    let var: (i32 i32 i32);
    @(1 2 3);
    @( (...)var); // also works! which is good
    := var; // would also work the same way (this operator is a convenient shorthand we provide
    // so := var basically acts the same as consuming the stack directly with (...)var;
}   // this way, our (...) operator is always the same, consistent operation.
    // it consumes the stack and pushes a tuple with what it consumed.

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
