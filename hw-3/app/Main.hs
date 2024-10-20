module Main where

import Lexer (tokenize)
import Parser (parseConfig)
import Xml (showConfig)

config :: String
config =
  "\
  \set asdf = 3\n\
  \#= asdf asd aosjd adh \n\
  \ sdasfsf =#\n\
  \set asdf2 = ?[asdf 1 -]\n\
  \set asdf3 = { asdf: ?[2 asdf print *] }"

main :: IO ()
main = do
  tokens <- case tokenize config of
    Right ok -> return ok
    Left err -> print err >> return []

  parsed <- case parseConfig tokens of
    Right ok -> return ok
    Left err -> print err >> return []

  putStrLn $ showConfig parsed
