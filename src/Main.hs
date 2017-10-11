import Bfi

main :: IO ()
main = do
  code <- getLine
  putStrLn (execute (parse code) (Model 0 0 (take 1000 (repeat 0)) 0))
