```cpp
// example of the HFS MIR language
fn foo: (i32 i32 str) -> (i32 i32 i32) {
  start:
    let var: f32;
    store var, 3.0; 
    jump if_block_0; // jumping with no values
    if_cond_0:
        branch var < 2.0, if_block_0, else_if_cond_0;
        if_block_0:
            let var1: i32;
            %inst0 = push (69); // where %inst0 is an InstId = 0 effectively 
            store var1, 3 + 5;
            let var2: i32;
            %inst1 = push (69);
            store var2, 3 * 66;
            %ret0 = jump end_if_0, (%inst0 %inst1);   // this came from tracking state in analysis
    else_if_cond_0:
        branch -420 < 5, else_if_block_0, else_if_cond_1;
        else_if_block_0:
            %inst2 = push (0 0 0);
            %ret1 = jump end_if_0, %inst2;   // this came from tracking state in analysis
    else_if_cond_1:
        branch -69 < 69, else_if_block_0, else_block_0;
        else_if_block_1:
            %ret2 = jump end_if_0, push (1 1 1);  // if you prefer this representation
            // here we put the push() directly in the jump (pick what you prefer)
    else_block_0:
        %inst4 = push (420 420 420);
        %ret3 = jump end_if_0, %inst4;       // this came from tracking state in analysis
  end_if_0:
    // here, phi will either deconstruct the arguments, or literally return a tuple
    // either way thats more implementation details than something to represent in IR
    %inst5 = phi (if_block_0, %ret0), (else_if_block_0, %ret1), (else_if_block_1, %ret2), (else_block_0, %ret3);
    let example: i32;
    store example, %inst4; // whatever phi returns at runtime
    jump while_cond_0;
    while_cond_0:
        branch example != 0, while_block_0, end_while_0;
        while_block_0:
            store example, example - 1;
            jump while_cond_0;
    end_while_0:
        jump end;
  end:
    return %inst5; // we know inst5 has the values we want
    // lets say it didnt: we might first build a local variable that contains our values
}

// same code but in henceforth
fn foo: (i32 i32) -> (i32 i32 i32) {
    @pop @pop;
    let var: f32; @(3.0) &= var;

    if @(var 2.0 <) {
        let var1: i32; 
        @(69 3 5 +)
        &= var1;
        let var2: i32;
        @(69 3 66 *)
        &= var2;
        @(69);
    } else if @(-420 5 >) { // always false just for demonstration
        @(0 0 0);
    } else if @(-69 69 >) { // always false just for demonstration
        @(1 1 1);
    } else {
        @pop @(420 420 420);
    }
    let example: i32; &= example;
    while @(example 0 !=) {
        @(example 1 -) &= example;
    }


}
fn main: () -> (i32) {
    @(1 2) &> foo;
    @pop @pop;
}
```

