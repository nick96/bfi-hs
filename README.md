# Bfi.hs

Brainfuck interpreter in Haskell.

Brainfuck is a simple language that emulate a Turing Machine. It is
made up of a series of cells that contain values which allow the
following operations on them: 

#. Increment value
#. Decrement value
#. Print character value
#. Read in character value

You can also increment and decrement the cell number you are using. To
make it a Turing Machine, Brainfuck also has a looping contruct. This
means there are only eight operators in brain fuck:

#. +: Increment cell value
#. -: Decrement cell value
#. >: Move to next cell
#. <: Move to previous cell
#. .: Print character value of current cell
#. ,: Read character value into current cell
#. [: Loop start
#. ]: Loop end, continue past if current cell's value is 0
