# Lexical Structure
This section describes the basic lexical elements that make up Henceforth source code.

## Comments
Comments are indicated by `//`.
```
// this is a comment
this is no longer part of the comment
```
## Identifiers
Function and variable identifiers can contain `a-z`, `A-Z`, `0-9` and `_`, except for the first character, which can't be a number.
```
add // valid
getValue2 // valid
_private // valid

2var // invalid (starts with number)
my-function // invalid (has hyphen)
hello world // invalid (has space)
```

## Literals
Integer literals represent whole numbers:
```
42
-17
1000000
```

Floating-point literals represent decimal numbers:
```
3.14
-0.5
2.0
```

String literals are enclosed in double quotes:
```
"Hello, World!"
""
"Line 1\n Line 2"
```

The following escape sequences are allowed:
```
\n newline
\t tab
\\ backslash
\" double quote
\r carriage return
```

Boolean literals represent truth values:
```
true
false
```

## Keywords
The following identifiers are reserved as keywords and can't be used as an identifier.
```
fn
while
continue
break
return
let
if
else
false
true
i32
f32
str
bool
@pop
@pop_all
@dup
@swap
@over
@rot
@rrot
@nip
@tuck
@rev
@depth
```

## Operators
Henceforth provides several categories of operators.

**Arithmetic Operators**
```
+  //  addition
-  //  subtraction
*  //  multiplication
/  //  division
%  //  modulo
```
**Comparison Operators**
```
== //  equal to
!= //  not equal to
<  //  less than
>  //  greater than
<= //  less than or equal to
>= //  greater than or equal to
```

**Logical Operators**
```
&& //  logical AND
|| //  logical OR
!  //  logical NOT
```

## Miscellaneous
```
@    // stack block indicator
()   // stack block delimiter
:    // function signature separator
->   // return type indicator/argument redirection
{}   // block delimiters
;    // statement terminator (optional)
:=   // copy top value to variable
&=   // pop to variable
:>   // copy arguments to function call   
&>   // move arguments to function call
[]   // array indexing operator
[:]= // copy value to array at a certain index
[&]= // move value to array at a certain index
```
