module Main where
import Lexer

main :: IO ()
main = do
  line <- getLine
  print $ tokenize line

