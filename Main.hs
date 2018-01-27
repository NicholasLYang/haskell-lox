module Main where
import Lexer
import Parser

main :: IO ()
main = do
  line <- getLine
  print $ tokenize line
  print $ parse $ tokenize line

