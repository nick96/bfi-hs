# Bfi.hs

Brainfuck interpreter in Haskell.

Brainfuck is a simple but Turing complete language. Like a Turing machine, its memory is a tape containing adjacent cells.
We can perform the following operations on each cell: 

1. Increment value
2. Decrement value
3. Print character value
4. Read in character value

There is also the ability to move the tape head left or right (like in a Turing Machine). Each of these operations is
represented by a single character:

1. +: Increment cell value
2. -: Decrement cell value
3. \>: Move to next cell
4. <: Move to previous cell
5. .: Print character value of current cell
6. ,: Read character value into current cell
7. [: Loop start
8. ]: Loop end, continue past if current cell's value is 0
