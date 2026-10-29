# Introduction

Henceforth is a programming language that aims to bridge the gap between imperative and stack-based languages, acting as a sort of softer introduction to the latter, as opposed to languages like Forth or Factor. 

This reference describes the Henceforth language, its syntax, its semantics, and the constructs available to a programmer when writing Henceforth. While it may provide small guided examples related to each construct, it is not a tutorial, and it is not a formal specification of the compiler or the language.
For a worked example, read the [last section of the reference](./tic-tac-toe.md)
If you want to look something up while you already know roughly what you're doing, this is the document for that.

## Who this is for

This reference assumes you've programmed before and are comfortable with basic ideas like variables, functions, and control flow. It does not assume prior experience with stack-based languages or other more advanced features of the language, as those will be explained thoroughly.

## How to read this reference

Each section documents one part of the language, generally moving from the smallests building blocks to larger ones. Sections are meant to be read in order the first time through, then looked independently afterwards.

Code examples will appear throughout the reference. Whenever an operation's effect on the stack matters, it's shown using stack notation in a comment (`// (stack before) -> (stack after)`), (always listing the entirety of the current stack, except for function and stack operation signatures, where only the relevant elements from the top of the stack are shown), where the top of the stack is to the right.

## Structure of this reference

- [Lexical Structure](./lexical-structure.md) - how source code is divided into its lexical elements: identifiers, literals, comments, keywords and operators
- [Types](./types.md) - the types available in Henceforth, their properties and how they're written
- [Stack Operations](./stack.md) - the operators and keywords that work directly on the stack
- [Variables](./variables.md) - declaring and using variables
- [Functions](./functions.md) - declaring functions and function call stack context-switching
- [Assignments and Calls](./assignments-and-calls.md) - binding values and invoking functions, and the copy/move semantics that affect both
- [Control Flow](./control-flow.md) - conditionals and loops
- [Arrays](./arrays.md) - array types, indexing, and bounds behavior
- [Builtins](./builtins.md) - list of functions available in Henceforth and their purpose
- [Tic-tac-toe](./tic-tac-toe.md) - a fully worked example of a more complex Henceforth program
