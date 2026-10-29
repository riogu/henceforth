# Arrays

Arrays are the primary way of grouping values in Henceforth. These are homogeneous collections (all elements must be the same type), and they can either have a fixed-length, or in very specific scenarios, can have an unknown length.

## Declaring arrays

```rs
let arr: [4]str;
```

Declaring an array variable is fairly straightforward. It takes a length between square brackets, which as of version 1.0.0 __must__ be an integer literal (no compile-time constants allowed as of now), and a type, which can be anything, including another array.

```rs
let arr: [5][3]i32; // this is an array of size 5, where each element is an array of size 3 of integers
```

> [!warning] 
> Arrays are initialized with junk values.

## Creating an array

An array can be created by either simply pushing an array literal to the stack, or by creating an array variable and assigning a value to each index.

Creating an array literal on the stack is done with the following syntax: 
```rs
@([1 2 3]) // () -> ([1 2 3]), the stack depth is 1
```

> [!NOTE]
> When creating an array literal, these are automatically __fixed-length__, and their length will always correspond to the length of the literal.

## Reading from an array

Henceforth deals with indexing in a less traditional manner than imperative languages. Instead of having a single operation that lets you read an index from an array, and then simply assigning to it to write to that index, there are two separate operations.

To read from an array, we use the `[]` operator.
```rs
let arr: [3]i32;
@([1 2 3]) &= arr; // arr: [1 2 3]
@(arr 0);
@([]); // (arr 0) -> (arr[0])
```

## Assigning to an array

Writing to an array in Henceforth uses the same philosophy as assigning to a variable or calling a function: there are two operators, one that copies the value to the array, and one that moves the value to it. This is further explained in [the next section](./assignments-and-calls).

```rs
let arr: [5]i32;
let idx: i32;
@(0) &= idx;
@(2 idx) [&]= arr; // arr[idx] = 2; Since we used the move operator, the stack is empty.
@(idx 1 +) &= idx;
@(3 idx) [:]= arr; // arr[idx] = 3; We used the copy operator so the stack has (3 idx)
@pop_all;
// Now, the array contains [2, 3, _, _, _].
```

> [!NOTE]
> In the given example, trying to read from indexes 2-4 will result in junk values being returned.

## Type-checking arrays

As mentioned earlier, there is one exception to arrays needing a fixed-length. When defining a function with an array argument or return value, it can be of unknown length. For example:
```rs
fn index_array: ([]i32) -> () {
  @(4 []); // if the array has size <= 4, this will not throw a compile-time error. (<arr>) -> (<arr[4]>)
  @pop;
}

fn main: () -> () {
  @([1 2 3]) &> index_array;
}
```
```rs
fn id_array: ([]i32) -> ([]i32) {}

fn id_array_sized: ([3]i32) -> ([3]i32) {}

fn main: () -> () {
  @([1 2 3]) &> id_array; // ([1 2 3]) -> ([1 2 3]), HOWEVER, the returned array is now of type []i32
  // this means that it cannot be printed, and will not throw a compile-time index out of bounds error
  let arr: [3]i32;
  := arr; // we can circumvent this by assigning it back into a variable, as this makes it sized again
  :> id_array_sized; // if the function returns a fixed-length array, it is already returned as such
  @pop_all;
}
```

Casting a fixed-length array into an unknown length array is always valid as long as the element types match. The inverse operation is also always valid, however, if the array turns out to have more elements than that fixed-length, it will only keep the first _N_ elements. If it has fewer elements, the leftover ones will be junk values.

Fixed-length array to fixed-length array type checking (e.g. passing it to a function with a fixed-length array argument, returning a fixed-length array, assigning to a variable) is only valid if both the lengths and the element types match.

## Bounds checking

Whenever possible, Henceforth will throw a compile-time error if it can detect an out-of-bounds access to an array. As of version 1.0.0, this only occurs if the index is an integer literal, and the array has a fixed-length (this is already enforced to be an integer literal).

```rs
fn main: () -> () {
  let arr: [4]i32;
  @([1 2 3 4]) &= arr;
  @(arr 5 []); // throws a compile-time error
  @pop;
  let n: i32;
  @(6) &= n;
  @(arr n []); // throws a run-time error
  @pop;
}
```

