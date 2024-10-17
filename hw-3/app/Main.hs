module Main where

import Data.Maybe (fromMaybe)
import Lexer
import Parser

main :: IO ()
main = do
  let tokens = fromMaybe [] $ tokenize "array(array(1,), {asdf: \"asd\", asdf2: array(),}, 2, 3)"
  let Parser parse = parseValue
  print $ parse tokens
