module Main where

import Lexer

main :: IO ()
main = do
  print $ tokenize "{ :asd} ,,, array() 123 \"string\""