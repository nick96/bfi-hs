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
data Model =
  Model Int -- Current value
        Int -- Current cell
        [Int] -- Cells

-- | Main entry point for the program.
main :: IO ()
main = putStrLn "Not implemented."

-- | Parse the Brainfuck file string into the `Construct` data type.
parse :: String -> [Construct]
parse [] = []
parse (c:cs)
  | c == '-' = DecVal : (parse cs)
  | c == '+' = IncVal : (parse cs)
  | c == '<' = PrevCell : (parse cs)
  | c == '>' = NextCell : (parse cs)
  | c == '.' = Print : (parse cs)
  | c == '[' = (Loop (parse loop)) : (parse afterLoop)
  where
    loop = breakAwayLoop cs
    afterLoop = skipLoop cs

-- | Break the loop away from the reset of the program.
breakAwayLoop :: String -> String
breakAwayLoop [] = ""
breakAwayLoop (c:cs)
  | c == ']' = ""
  | otherwise = c : (breakAwayLoop cs)

-- | Skip over the loop code.
skipLoop :: String -> String
skipLoop "" = ""
skipLoop (c:cs)
  | c == ']' = cs
  | otherwise = skipLoop cs

-- | Execute the Brainfuck program represented by the `Construct` list.
execute :: [Construct] -> Model -> String
execute [] _ = ""
execute (x:xs) (Model val index cells)
  | x == Print = (chr val) : (execute xs (Model val index cells))
  | otherwise = execute xs (execConstruct x (Model val index cells))

-- | Execute a singular construct and return its effects on the model.
execConstruct :: Construct -> Model -> Model
execConstruct DecVal (Model val index cells) = Model (val - 1) index cells
execConstruct IncVal (Model val index cells) = Model (val + 1) index cells
execConstruct PrevCell (Model val index cells) =
  Model (getNth newIndex cells) newIndex updatedCells
  where
    newIndex = index - 1
    updatedCells = updateNth index val cells
execConstruct NextCell (Model val index cells) =
  Model (getNth newIndex cells) newIndex updatedCells
  where
    newIndex = index + 1
    updatedCells = updateNth index val cells
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
loopOp original [] (Model val index cells)
  | val == 0 = (Model val index cells) -- End of loop
  | otherwise = loopOp original original (Model val index cells)
loopOp orginal (x:xs) model = loopOp orginal xs (execConstruct x model)
