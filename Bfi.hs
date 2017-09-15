{-| Brainfuck interpreter.

This is an interpreter for the Brainfuck language, as defined in at
<http://esolangs.org/wiki/brainfuck>.

= Brainfuck language:

    [@+@] Increments the current cell.

    [@>@] Move the current cell to the right.

    [@<@] Move the current cell to the left.

    [@-@] Decrements the current cell.

    [@.@] Print the character indicated my the current cell value.

    [@,@] Input a character an store it in the current cell.

    [@\[@] Jump past the matching \], if the current cell value is 0.

    [@\]@] If current cell value is non-zero, go back to the matching \].

Anything other than the above characters is considered a comment.

The current cell value begins at zero.

= Interpreter process:

This interpreter works in two stages. The first stage parses the the
program, breaking it up into a list of `Construct`s. The second stage
then goes over this list, executing the appropriate functions. My
rational for this two step process, rather than just going straight to
the execution stage, is because it gives me more flexibility. I can
include optimizations without having to rewrite everything and in the
future I'll be able to turn it into a JIT compiler, or even a straight
up compiler.

= TODO

  - Add pre-processor to build the cell list
  - Allow reading of file
  - Allow conversion of IR to machine code
  - Fix interpretation of complex bf program:

>++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+.
-}
module Bfi where

import Data.Char

-- | Structure to represent the data constructs in a Brainfuck program.
data Construct
  = DecVal
  | IncVal
  | NextCell
  | PrevCell
  | Print
  | Loop [Construct]
  deriving (Show, Eq)

-- | Modelling of the program state.
data Model = Model
  { currVal :: Int
  , currCell :: Int
  , cells :: [Int]
  , nesting :: Int
  }

-- | Parse the Brainfuck file string into the `Construct` data type.
-- Bug: Doesn't parse nested loops properly
parse :: String -> [Construct]
parse [] = []
parse (c:cs)
  | c == '-' = DecVal : (parse cs)
  | c == '+' = IncVal : (parse cs)
  | c == '<' = PrevCell : (parse cs)
  | c == '>' = NextCell : (parse cs)
  | c == '.' = Print : (parse cs)
  | c == '[' = (Loop (parseLoop 0 cs)) : (parse (skipLoop 0 0 cs))
  | otherwise = parse cs -- Ignore all other characters

-- | Parse loop, it is a bit different to the normal parser because it
-- ends at ']' rather than the end of the program. In the future it
-- might be good idea to generalise and put them in the same function
-- but this works for now.
parseLoop :: Int -> String -> [Construct]
parseLoop 0 [] = []
parseLoop _ [] = error "parseLoop: Unbalanced loop"
parseLoop nest (c:cs)
  | c == '-' = DecVal : (parseLoop nest cs)
  | c == '+' = IncVal : (parseLoop nest cs)
  | c == '<' = PrevCell : (parseLoop nest cs)
  | c == '>' = NextCell : (parseLoop nest cs)
  | c == '.' = Print : (parseLoop nest cs)
  | c == '[' =
    Loop (parseLoop (nest + 1) cs) :
    (parseLoop (nest + 1) (skipLoop nest (nest + 1) cs))
  | c == ']' = []
  | otherwise = parseLoop nest cs

-- | Skip over the loop code.
skipLoop :: Int -> Int -> String -> String
skipLoop _ _ [] = error "skipLoop: Unbalanced loop"
skipLoop origNest nest (c:cs)
  | c == ']' && nest == origNest = cs
  | c == ']' && nest /= origNest = skipLoop origNest (nest - 1) cs
  | c == '[' = skipLoop origNest (nest + 1) cs
  | otherwise = skipLoop origNest nest cs

-- | Execute the Brainfuck program represented by the `Construct` list.
execute :: [Construct] -> Model -> String
execute [] _ = ""
execute (x:xs) model
  | x == Print = (chr (currVal model)) : (execute xs model)
  | otherwise = execute xs (execConstruct x model)

-- | Execute a singular construct and return its effects on the model.
execConstruct :: Construct -> Model -> Model
execConstruct DecVal model = model {currVal = (currVal model) - 1}
execConstruct IncVal model = model {currVal = (currVal model) + 1}
execConstruct PrevCell model =
  model
  { currVal = (getNth ((currCell model) - 1) (cells model))
  , currCell = (currCell model) - 1
  , cells = (updateNth (currCell model) (currVal model) (cells model))
  }
execConstruct NextCell model =
  model
  { currVal = (getNth ((currCell model) + 1) (cells model))
  , currCell = (currCell model) + 1
  , cells = (updateNth (currCell model) (currVal model) (cells model))
  }
execConstruct (Loop loop) model = loopOp loop loop model
execConstruct Print _ = error "Reached pattern that should not be reached."

-- | Get the nth value from a list
getNth :: Int -> [a] -> a
getNth 0 (x:_) = x
getNth _ [] = error "nth element of list does not exist."
getNth n (_:xs) = getNth (n - 1) xs

-- | Update the nth value in a list
updateNth :: Int -> a -> [a] -> [a]
updateNth 0 newVal (_:xs) = newVal : xs
updateNth _ _ [] = error "nth element of list does not exist."
updateNth n newVal (x:xs) = x : (updateNth (n - 1) newVal xs)

-- | Evaluate a loop construct
loopOp :: [Construct] -> [Construct] -> Model -> Model
loopOp original [] model
  | (currVal model) == 0 = model -- End of loop
  | otherwise = loopOp original original model
loopOp orginal (x:xs) model = loopOp orginal xs (execConstruct x model)
