module Main where

import Control.Monad ((>=>))
import Data.Char (isDigit, isSpace)
import Data.List (stripPrefix)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

data LiteralValue
  = String String
  | Number Integer
  deriving (Show)

data Token
  = Set
  | ArrayBegin
  | ArrayEnd
  | DictBegin
  | DictSep
  | DictEnd
  | Delimiter
  | ExprBegin
  | ExprEnd
  | Comment String
  | Identifier String
  | Literal LiteralValue
  deriving (Show)

newtype Lexer = Lexer (String -> Maybe (Token, String))

instance Semigroup Lexer where
  (Lexer left) <> (Lexer right) =
    Lexer
      ( \text -> case left text of
          Just res -> Just res
          Nothing -> right text
      )

instance Monoid Lexer where
  mempty = Lexer (const Nothing)

splitOnce :: String -> String -> Maybe (String, String)
splitOnce delim text = case stripPrefix delim text of
  Just rest -> Just ("", rest)
  Nothing -> case text of
    "" -> Nothing
    (x : rest) -> do
      (left, right) <- splitOnce delim rest
      return (x : left, right)

lexSimple :: String -> Token -> Lexer
lexSimple prefix token =
  Lexer (stripPrefix prefix >=> (\rest -> Just (token, rest)))

lexComment :: Lexer
lexComment =
  Lexer
    ( \text -> do
        rest <- stripPrefix "#=" text
        (comment, rest') <- splitOnce "=#" rest
        return (Comment comment, rest')
    )

lexIdentifier :: Lexer
lexIdentifier =
  Lexer
    ( \text -> do
        identifier <- case (text =~ "^[a-zA-Z][_a-zA-Z0-9]*" :: String) of
          "" -> Nothing
          str -> Just str
        rest <- stripPrefix identifier text
        return (Identifier identifier, rest)
    )

lexNumber :: Lexer
lexNumber =
  Lexer
    ( \text -> do
        let digits = takeWhile isDigit text
        number <- readMaybe digits
        rest <- stripPrefix digits text
        return (Literal (Number number), rest)
    )

lexString :: Lexer
lexString =
  Lexer
    ( \text -> do
        rest <- stripPrefix "\"" text
        (string, rest') <- splitOnce "\"" rest
        return (Literal (String string), rest')
    )

nextToken :: String -> Maybe (Token, String)
Lexer nextToken =
  mconcat
    [ lexSimple "set" Set,
      lexSimple "array(" ArrayBegin,
      lexSimple ")" ArrayEnd,
      lexSimple "{" DictBegin,
      lexSimple ":" DictSep,
      lexSimple "}" DictEnd,
      lexSimple "," Delimiter,
      lexSimple "?[" ExprBegin,
      lexSimple "]" ExprEnd,
      lexComment,
      lexIdentifier,
      lexNumber,
      lexString
    ]

tokenize :: String -> Maybe [Token]
tokenize "" = Just []
tokenize text = do
  (token, rest) <- nextToken (dropWhile isSpace text)
  tokens <- tokenize rest
  return (token : tokens)

main :: IO ()
main = do
  print $ tokenize "{ :asd} ,,, array() 123 \"string\""