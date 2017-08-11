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
    | Input
    | Loop [Construct]
    deriving (Eq)

-- | Modelling of the program state.
data Model =
    Model Int
          Int
          [Int]

-- | Main entry point for the program.
main :: IO ()
main = putStrLn "Not implemented."

-- | Parse the Brainfuck file string into the `Construct` data type.
parse :: String -> [Construct]
parse _ = [DecVal]

-- | Execute the Brainfuck program represented by the `Construct` list.
execute :: [Construct] -> Model -> String
execute [] _ = ""
execute (x:xs) (Model val index cells)
    | x == DecVal = execute xs (Model (val - 1) index cells)
    | x == IncVal = execute xs (Model (val + 1) index cells)
    | x == NextCell = execute xs (Model 0 (index + 1) (updateNth index cells))
    | x == PrevCell = execute xs (Model 0 (index - 1) (updateNth index cells))
    | x == Print = (chr val) : (execute xs (Model val index cells))
    | x == Input = do
        c <- getChar
        execute xs (Model (ord c) index cells)
    | x == Loop loop = execute xs (loopOp loop loop (Model val index cells))

updateNth :: Int -> [Int] -> [Int]
updateNth _ xs = xs

loopOp :: [Construct] -> [Construct] -> Model -> Model
loopOp original curr (Model 0 index cells) = (Model 0 index cells)
loopOp _ _ _ = Model 0 0 []
