# henceforth

[![Build](https://github.com/riogu/henceforth/actions/workflows/rust.yml/badge.svg)](https://github.com/riogu/henceforth/actions/workflows/rust.yml)

#### A statically-typed stack-based programming language with an imperative twist.

> **Status:** work in progress. Version 1.0 coming soon.

## What Is Henceforth?

The stack-based language world can seem scary for most programmers. Henceforth aims to ease that transition by combining imperative language features with a stack-based approach.
This is achieved by several features, including:
1. Static typing and compile-time verified stack consistency
2. Move and copy semantics for assignments and function calls, made explicit at every use
3. Familiar control flow structure despite the stack-based core
4. Simple type system with primitive types (`i32`, `f32`, `bool`, `str`) and arrays

## How It Works

Internally, Henceforth has a hand-written frontend (lexer, recursive-descent parser, two-pass stack/semantic analyzer) into an SSA intermediate representation akin to LLVM IR, where optimization passes (Mem2Reg, DCE, CleanCFG, etc.) run before interpretation. A Cranelift backend is also coming in the near future.

## Getting Started

The quickest way to try Henceforth is building the binary directly from source.
Run the following commands:
```
$ git clone https://github.com/riogu/henceforth.git
$ cd henceforth
$ cargo install --path .
```
This will install the `henceforth` binary directly to your path.

Then, you can write your code in a `.hfs` file and run the Henceforth interpreter:
```v
$ cat helloworld.hfs
fn main: () -> () {
    @("Hello, world!\n") &> print;
}
$ henceforth helloworld.hfs
Hello, world!
```

When version 1.0 comes out, Henceforth will be available on `crates.io` and other package managers.

## Example

```rust
fn bubble_sort: ([]i32) -> ([]i32) {
    let arr: []i32; &= arr;
    let i: i32; @(0) &= i;

    while @(i 5 !=) {
        let j: i32; @(0) &= j;
        while @(j 4 i - !=) {
            if @(arr j [] arr j 1 + [] >) {
                let tmp: i32; @(arr j []) &= tmp;
                @(arr j 1 + [] j) [&]= arr;
                @(tmp j 1 +) [&]= arr;
            }
            @(j 1 +) &= j;
        }
        @(i 1 +) &= i;
    }
    @(arr);
}
fn main: () -> () {
    let arr: [5]i32;
    @(5 0) [&]= arr;
    @(3 1) [&]= arr;
    @(4 2) [&]= arr;
    @(1 3) [&]= arr;
    @(2 4) [&]= arr;
    @(arr) &> bubble_sort;
    &> print;
}
```
## Documentation

Language reference and usage docs can be found [here](https://riogu.github.io/henceforth).

## Command Line Usage

Henceforth has a very simple CLI:
```
Usage: henceforth <SOURCE>
```

## Reporting a Bug

If you find a bug, check the open issues to see if it has already been reported. If not, feel free to open a new issue.
If you have a fix for the bug, you can open a pull request with your changes. Our team is always happy to help with anything and answer all questions.

If your PR has been waiting for a while, you can ping @riogu or @joao-novo.
