import Bfi
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn (execute (parse (head args)) (Model 0 0 (take 1000 (repeat 0)) 0))
