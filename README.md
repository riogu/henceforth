# henceforth

[![Build](https://github.com/riogu/henceforth/actions/workflows/rust.yml/badge.svg)](https://github.com/riogu/henceforth/actions/workflows/rust.yml)
[![crates.io](https://img.shields.io/crates/v/henceforth.svg)](https://crates.io/crates/henceforth)

#### A statically-typed stack-based programming language with an imperative twist.

For an overview of the language and how the compiler works, see the [v1.0 release writeup](<link to your blog post>).

## What Is Henceforth?

The stack-based language world can seem scary for most programmers. Henceforth aims to ease that transition by combining imperative language features with a stack-based approach.
This is achieved by several features, including:
1. Static typing and compile-time verified stack consistency
2. Move and copy semantics for assignments and function calls, made explicit at every use
3. Familiar control flow structure despite the stack-based core
4. Simple type system with primitive types (`i32`, `f32`, `bool`, `str`) and arrays

## How It Works

Internally, Henceforth has a hand-written frontend (lexer, recursive-descent parser, two-pass stack/semantic analyzer) that compiles to an SSA intermediate representation akin to LLVM IR, where optimization passes (Mem2Reg, DCE, CleanCFG, etc.) run before either backend sees it. A [Cranelift](https://cranelift.dev/) backend compiles that IR to a real native executable, and is the default (`--backend cranelift`). It was the main intended target of the compiler from the start, since Henceforth was designed with the goal of compiling a stack language, rather than interpreting it. The original tree-walking interpreter (`--backend interpret`) is kept as a reference implementation.

## Getting Started

Install Henceforth from crates.io:
```
$ cargo install henceforth
```
Or build it from source:
```
$ git clone https://github.com/riogu/henceforth.git
$ cd henceforth
$ cargo install --path .
```
Either way, this installs the `henceforth` binary to your path. Compiling programs also requires a C toolchain (`cc`, `clang` or `gcc`) for linking.

Then, you can write your code in a `.hfs` file and run it:
```v
$ cat helloworld.hfs
fn main: () -> () {
    @("Hello, world!\n") &> print;
}
$ henceforth helloworld.hfs
$ ./a.out
Hello, world!
```

## Example

```rust
fn bubble_sort: ([]i32 i32) -> ([]i32) {
    let N: i32; &= N;
    let arr: [N]i32; &= arr;

    let i: i32; @(0) &= i;
    while @(i N !=) {
        let j: i32; @(0) &= j;
        while @(j N 1 - i - !=) {
            if @(arr j [] arr j 1 + [] >) {
                let tmp: i32; @(arr j []) &= tmp;
                @(arr j 1 + [] j) [&]= arr;
                @(tmp j 1 +)      [&]= arr;
            }
            @(j 1 +) &= j;
        }
        @(i 1 +) &= i;
    }
    @(arr)
}

fn main: () -> () {
    let arr: [5]i32;
    @([5 3 4 1 2]) &= arr;
    @(arr 5) &> bubble_sort &> print;
}
```

## Documentation

Language reference and usage docs can be found [here](https://riogu.github.io/henceforth).

(... rest unchanged: Contributing, Command Line Usage, Reporting a Bug ...)
