## Code examples
```cpp
// example of the CFG IR language
fn foo: (i32 i32 str) -> (i32 i32 i32) {
  start:
    let var: f32;
    assign var, 3.0; 
    jump if_block_0, (); // jumping with no values
    if_cond_0:
        branch var < 2.0, if_block_0, else_if_cond_0;
        if_block_0:
            let var1: i32;
            push (69);
            assign var1, 3;
            let var2: i32;
            push (69);
            assign var2, 3;
            jump end, (69 69);   // this came from tracking state in analysis
    else_if_cond_0:
        branch var < 2.0, else_if_block_0, else_if_cond_1;
        else_if_block_0:
            branch -420 < 5, 
            push (0 0 0);
            jump end, (0 0 0);   // this came from tracking state in analysis
    else_if_cond_1:
        branch var < 2.0, else_if_block_0, else_cond_0;
        else_if_block_1:
            branch -420 < 5, 
            push (1 1 1);
            jump end, (1 1 1);   // this came from tracking state in analysis
    else_cond_0:
        else_block_0:
            push (420 420 420);
            jump end, (420 420 420); // this came from tracking state in analysis
  end:
    // here, phi will either deconstruct the arguments, or literally return a tuple
    // either way thats more implementation details than anything
    return phi (if_block_0, else_if_block_0, else_if_block_1, else_block_0);
}

// same code but in henceforth
fn foo: (i32 i32) -> (i32 i32 i32) {
    @pop @pop;
    let var: f32; @(3.0) &= var;

    if @(var 2.0 <) {
        let var1: i32; 
        @(69 3)
        &= var1;
        let var2: i32;
        @(69 3)
        &= var2;
        @(69);
    } else if @(-420 5 >) { // always false just for demonstration
        @(0 0 0);
    } else if @(-69 69 >) { // always false just for demonstration
        @(1 1 1);
    } else {
        @pop @(420 420 420);
    }
}
fn main: () -> (i32) {
    @(1 2) &> foo;
    @pop @pop;
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
