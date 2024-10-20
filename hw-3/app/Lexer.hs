module Lexer
  ( LiteralValue (..),
    Token (..),
    LexingError (..),
    tokenize,
  )
where

import Control.Monad ((>=>))
import Data.Char (isDigit, isSpace)
import Data.List (stripPrefix)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Util (maybeToEither, splitOnce)

data LiteralValue
  = String String
  | Number Integer
  deriving (Show, Eq)

data Token
  = Set
  | Equals
  | ArrayBegin
  | ArrayEnd
  | DictBegin
  | DictSep
  | DictEnd
  | Delimiter
  | ExprBegin
  | Add
  | Subtract
  | Multiply
  | Print
  | ExprEnd
  | Identifier String
  | Literal LiteralValue
  deriving (Show, Eq)

data LexingError
  = NoValidToken String
  | UnclosedComment String
  deriving (Show)

newtype Lexer = Lexer (String -> Either LexingError (Token, String))

instance Semigroup Lexer where
  (Lexer left) <> (Lexer right) = Lexer $ \text -> case left text of
    Right res -> Right res
    _ -> right text

instance Monoid Lexer where
  mempty = Lexer $ Left . NoValidToken

stripPrefix' :: String -> String -> Either LexingError String
stripPrefix' pref text = maybeToEither (NoValidToken text) $ stripPrefix pref text

splitOnce' :: String -> String -> Either LexingError (String, String)
splitOnce' delim text = maybeToEither (NoValidToken text) $ splitOnce delim text

lexSimple :: String -> Token -> Lexer
lexSimple prefix token = Lexer $ stripPrefix' prefix >=> (\rest -> Right (token, rest))

lexIdentifier :: Lexer
lexIdentifier = Lexer $ \text -> do
  identifier <- case (text =~ "^[a-zA-Z][_a-zA-Z0-9]*" :: String) of
    "" -> Left $ NoValidToken text
    str -> Right str

  rest <- stripPrefix' identifier text
  return (Identifier identifier, rest)

lexNumber :: Lexer
lexNumber = Lexer $ \text -> do
  let digits = takeWhile isDigit text
  number <- maybeToEither (NoValidToken text) $ readMaybe digits
  rest <- stripPrefix' digits text
  return (Literal (Number number), rest)

lexString :: Lexer
lexString = Lexer $ \text -> do
  rest <- stripPrefix' "\"" text
  (string, rest') <- splitOnce' "\"" rest
  return (Literal (String string), rest')

nextToken :: String -> Either LexingError (Token, String)
Lexer nextToken =
  mconcat
    [ lexSimple "set" Set,
      lexSimple "=" Equals,
      lexSimple "array(" ArrayBegin,
      lexSimple ")" ArrayEnd,
      lexSimple "{" DictBegin,
      lexSimple ":" DictSep,
      lexSimple "}" DictEnd,
      lexSimple "," Delimiter,
      lexSimple "?[" ExprBegin,
      lexSimple "+" Add,
      lexSimple "-" Subtract,
      lexSimple "*" Multiply,
      lexSimple "print" Print,
      lexSimple "]" ExprEnd,
      lexIdentifier,
      lexNumber,
      lexString
    ]

stripAndDropComment :: String -> Either LexingError String
stripAndDropComment text =
  let stripped = dropWhile isSpace text
   in case stripPrefix "#=" stripped of
        Nothing -> Right stripped
        Just rest -> case splitOnce "=#" rest of
          Nothing -> Left $ UnclosedComment text
          Just (_, rest') -> stripAndDropComment rest'

tokenize :: String -> Either LexingError [Token]
tokenize "" = Right []
tokenize text = do
  stripped <- stripAndDropComment text
  (token, rest) <- nextToken stripped
  tokens <- tokenize rest
  return (token : tokens)
