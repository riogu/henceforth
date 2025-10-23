## Code examples
```cpp
// @() blocks are used to do operations on the stack
fn foo: (i32 i32) -> (i32) {
    let var1: i32;
    let var2: i32;
    @((3213 123)foo 23 +)
    := var1; // copy
    @(var1 123 +) &= var2; // move

    if @(69420 ==) { // left value in the stack
        @(69) return; 
    } elif @(123 ==) {
        @(233321) return;
    } elif @(123 ==) {
    } else {
        @pop; // pop the stack
        // @(pop);
    }
}

fn somefunc: () -> (bool str i32 i32 i32 i32) {
    @(false "some string thing" 6969696 312 3 12) 
    // functions return whatever changes they made to the stack 
}

fn main: (i32) -> i32 {
    let count: i32;
    let sum: i32;

    (123 123)foo;
    &= count;

    @( ()somefunc );

    while @(typeof int !=) {
        @(sum +) &= sum;
    }
}
```
