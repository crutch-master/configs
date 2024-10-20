module Main where

import Lexer (tokenize)
import Parser (Config (..), parseConfig)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Xml (showConfig)

main :: IO ()
main = do
  args <- getArgs

  (src, target) <- case args of
    [src, target] -> return (src, target)
    _ -> putStrLn "expected two arguments" >> exitFailure

  text <- readFile src

  tokens <- case tokenize text of
    Right tokens -> return tokens
    Left err -> putStrLn ("unable to tokenize:\n" <> show err) >> exitFailure

  config <- case parseConfig tokens of
    Right config -> return config
    Left err -> putStrLn ("unable to parse:\n" <> show err) >> exitFailure

  let Config _ logs = config

  writeFile target $ showConfig config
  putStrLn $ unlines logs
