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
}

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



// Calculate the nth Fibonacci number
fn fibonacci: (i32) -> (i32) {
    let n: i32;
    let a: i32;
    let b: i32;
    let temp: i32;
    let count: i32;
    
    &= n;           // Pop input
    @(0) &= a;      // Initialize a = 0
    @(1) &= b;      // Initialize b = 1
    @(0) &= count;  // Initialize counter
    
    // Handle edge cases
    while @(n 1 >) {
        @(a b +) &= temp;  // temp = a + b
        @(b) &= a;         // a = b
        @(temp) &= b;      // b = temp
        @(count 1 +) &= count;
        
        // Early exit when we've computed n-1 iterations
        @(count n 1 - >=) if {
            break;
        }
    }
    
    // Return appropriate value
    @(n 0 ==) if {
        @(a);  // fib(0) = 0
    } else {
        @(b);  // fib(n) = b
    }
}

// Main program: calculate and return fib(10)
fn main: () -> (i32) {
    @(10) &> fibonacci;
    // Result is now on stack, will be returned
}
```
```py 
# Simple and Efficient Construction of Static Single Assignment Form (Braun et. al)
    writeVariable(variable, block, value):
        currentDef[variable][block] ← value
    
    readVariable(variable, block):
        if currentDef[variable] contains block:
            # local value numbering
            return currentDef[variable][block]
        # global value numbering
        return readVariableRecursive(variable, block)
    
    readVariableRecursive(variable, block):
        if block not in sealedBlocks:
            # Incomplete CFG
            val ← new Phi(block)
            incompletePhis[block][variable] ← val
        else if |block.preds| = 1:
            # Optimize the common case of one predecessor: No phi needed
            val ← readVariable(variable, block.preds[0])
        else:
            # Break potential cycles with operandless phi
            val ← new Phi(block)
            writeVariable(variable, block, val)
            val ← addPhiOperands(variable, val)
        writeVariable(variable, block, val)
        return val
    
    addPhiOperands(variable, phi):
        # Determine operands from predecessors
        for pred in phi.block.preds:
            phi.appendOperand(readVariable(variable, pred))
        return tryRemoveTrivialPhi(phi)
    
    tryRemoveTrivialPhi(phi):
        same ← None
        for op in phi.operands:
            if op = same || op = phi:
                continue  # Unique value or self-reference
            if same ≠ None:
                return phi  # The phi merges at least two values: not trivial
            same ← op
        if same = None:
            same ← new Undef()  # The phi is unreachable or in the start block
        users ← phi.users.remove(phi)  # Remember all users except the phi itself
        phi.replaceBy(same)  # Reroute all uses of phi to same and remove phi
        # Try to recursively remove all phi users, which might have become trivial
        for use in users:
            if use is a Phi:
                tryRemoveTrivialPhi(use)
        return same
    
    sealBlock(block):
        for variable in incompletePhis[block]:
            addPhiOperands(variable, incompletePhis[block][variable])
        sealedBlocks.add(block)
```
