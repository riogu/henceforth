# Tic-Tac-Toe

To show the core ideas of Henceforth, let's walk through a worked example of a Tic-Tac-Toe program.

## Setting the stage

We will represent the board as an array with 9 strings, where `" "` will represent an empty square, and `"X", "O"` represent each player's moves. For convenience, we will reference the player that uses `X` as player 1.  

We define our main function, create our `board` array, and fill it with `" "` strings. This represents an empty board.
```rs
fn main: () -> () {
    let board: [9]str;

    // to fill the board, we iterate over each of its indices using a while loop, and use a copy array assignment to fill the array
    let idx: i32; @(0) &= idx;
    @(" " idx);
    while @(idx 9 !=) {
        // the stack state is currently (" " idx)
        [:]= board;
        // using a copy array assignment allows us to increment `idx`
        // without pushing it repeatedly to the stack, while keeping " " in the stack for the whole duration of the loop
        @(1 +) := idx;
    }
    @pop_all; // after the loop, the stack state is (" " idx), so we pop these values as they are no longer useful
    @("\nBeginning Tic-Tac-Toe:\n") &> print;
}
```
## Printing the board

Next, we print our board so the user can see the initial board state:
```rs
    ...
    @(board) &> print_board;
```
Now, we define `print_board` as follows:
```rs
fn print_board: ([9]str) -> () {
    let board: [9]str; &= board;
    let idx: i32; @(0) &= idx;
    @("\n") &> print;

    // we iterate over each index in the board
    while @(idx 9 !=) {
        // every 3 indices, starting from the first one, we print a separating line
        if @(idx 3 % 0 ==) { @(" |-----------|\n") &> print; }


        // for every index we print a vertical bar, to represent the left border of the slot,
        // then index into the array and print the value
        @(" | ") &> print; @(board idx []) &> print;

        // at the end of each line, we print the right border
        if @(idx 3 % 2 ==) { @(" |\n") &> print; }

        @(idx 1 +) &= idx;
    }
    // we finish the board by printing the final separating line
    @(" |-----------|\n\n") &> print
}
```

## The game loop

Next, back in `main`, we define the game loop:

```rust
    ...
    let is_player1_turn: bool; @(true) &= is_player1_turn;
    // while a win hasn't been found yet with the current board, we loop
    // we use a copy call because `board` will be used inside the loop
    while @(board) :> game_not_ended {
        // here, the stack contains (board)
        @("please input a move") &> print;
        // depending on which player's turn it is, we print the corresponding prompt, and add its "piece" to the stack.
        if @(is_player1_turn) { 
            @(" (player X's turn):\n") &> print; @("X")
        } else {
            @(" (player O's turn):\n") &> print; @("O")
        }
        // the stack now contains (board "X"/"O")
        // we get the position from user input
        :> input_int
        // then we pass it to a function called `tic_tac_toe`
        // at this point, the stack contains (board "X"/"O" position)
        &> tic_tac_toe
        // we need a way to update the board, since functions can't mutate arrays that are passed in,
        // `tic_tac_toe` must return the new board
        &= board;
        // with all this information, we can infer that `tic_tac_toe`'s signature will be ([9]str str i32) -> ([9]str)
        @(is_player1_turn !) &= is_player1_turn; // we swap the current player
    }
```
## Checking for a win

We define `game_not_ended` as a function that checks for a win from each player, using a helper function called `player_win`, and they are defined as such:
```rs
fn player_win: ([9]str str) -> (bool) {
    let player: str; &= player;
    let board: [9]str; &= board;
    // for each of the possible win configurations for the current player,
    // we check if all of those positions have the current player string
    //
    // each line of this stack block will produce a single boolean that represents
    // whether a win was found for that configuration
    // and the last line checks if any of them are true
    @(
      // horizontal win
      board 0 [] player ==  board 1 [] player == && board 2 [] player == &&
      board 3 [] player ==  board 4 [] player == && board 5 [] player == &&
      board 6 [] player ==  board 7 [] player == && board 8 [] player == &&
      // vertical win
      board 0 [] player ==  board 3 [] player == && board 6 [] player == &&
      board 1 [] player ==  board 4 [] player == && board 7 [] player == &&
      board 2 [] player ==  board 5 [] player == && board 8 [] player == &&
      // diagonal win
      board 0 [] player ==  board 4 [] player == && board 8 [] player == &&
      board 2 [] player ==  board 4 [] player == && board 6 [] player == &&
      // at this point there will be 8 booleans in the stack, and we "or" them,
      // so that if any of them are true (a win is found), the result is true
      || || || || || || ||
     )
}

// `game_not_ended` is trivially defined, we check for a win from either player, and return false if one is found, otherwise we return true
// note that this doesn't handle ties, and will loop infinitely if one occurs
// as an exercise, you can try to implement this yourself
fn game_not_ended: ([9]str) -> (bool) {
    if @("X") &> player_win {
        @(false)
    } else if @("O") &> player_win {
        @(false)
    } else {
        @pop @(true)
    }
}
```
## Showing the result to the user

Back in `main`, after the while loop, we know that a win has been found, so we just check which player won, and print it.
```rust
    ...
    @(board)
    // the logic behaves similarly to `game_not_ended`, but instead, we add a string to the stack
    // saying which player won, then print it at the end
    if @("X") &> player_win {
        @("the winner was player 'X'!\n") 
    } else if @("O") &> player_win {
        @("the winner was player 'O'!\n") 
    }
    &> print;
```

If you run this program, you get an interactive Tic-Tac-Toe program, that looks something like this:
```
Beginning Tic-Tac-Toe:

 |-----------|
 |   |   |   |
 |-----------|
 |   |   |   |
 |-----------|
 |   |   |   |
 |-----------|

please input a move (player O's turn):
1

 |-----------|
 | O |   |   |
 |-----------|
 |   |   |   |
 |-----------|
 |   |   |   |
 |-----------|

please input a move (player X's turn):
2

 |-----------|
 | O | X |   |
 |-----------|
 |   |   |   |
 |-----------|
 |   |   |   |
 |-----------|

```
