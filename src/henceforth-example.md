## Code examples
```cpp
import hfs;



fn foo: (i32 i32 str) -> (i32 i32 i32) {

    let var: f32;
    @(1 2 3.0) &= var;
    @(1 2 3.0) := var;
    @(1 2 3.0) &> func;
    @(1 2 3.0) :> func;

}


fn func: (i32 i32 f32) -> (str i32 i32 i32) {
    let var: i32; // file_name%func()::var
    {
        let var: i32; // file_name%func()::0::var
        {
            let var: i32; // file_name%func()::0::0::var
        }
        {
            let var: i32; // file_name%func()::0::1::var
        }
    }
    @( 1 2 + 4 *); // is just one multiplication "node"
    // so internally our stack block sees
    @( ((1 2) + 4) *) ) // which turns it into a compilable language lol
    @( mul(add(1, 2), 4))
}

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
        @((temp)factorial);
        @(sum +) &= sum;  // pop and assign
    }
    
    @(sum) return;  // Returns 153 (1! + 2! + 3! + 4! + 5!)
}
```
