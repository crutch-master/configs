module Main where

import Data.Maybe (fromMaybe)
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
  let tokens = fromMaybe [] $ tokenize config
  case parseConfig tokens of
    Just parsed -> putStrLn $ showConfig $ toXmlConfig parsed
    _ -> print "not parsed!!!"