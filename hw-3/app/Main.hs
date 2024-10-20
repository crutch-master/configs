module Main where

import Lexer (tokenize)
import Parser (parseConfig)
import Xml (showConfig, toXmlConfig)

config :: String
config =
  "\
  \set asdf = array(array(1,), {asdf: \"asd\", asdf2: array(),}, 2, 3)\n\
  \#= asdf asd aosjd adh \n\
  \ sdasfsf =#\n\
  \set asdf2 = {asd: 5}"

main :: IO ()
main = do
  tokens <- case tokenize config of
    Right ok -> return ok
    Left err -> print err >> return []

  parsed <- case parseConfig tokens of
    Right ok -> return ok
    Left err -> print err >> return []

  let xmlConfig = toXmlConfig parsed
      result = showConfig xmlConfig

  putStrLn result
