{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Monad (ap, liftM)
import Data.Maybe (listToMaybe)
import qualified Lexer

data Value
  = Array [Value]
  | Dict [(String, Value)]
  | Number Integer
  | String String
  deriving (Show)

newtype Parser a = Parser ([Lexer.Token] -> Maybe (a, [Lexer.Token]))

data ConfigEntry
  = Comment String
  | Set (String, Value)
  deriving (Show)

type Config = [ConfigEntry]

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure val = Parser $ \tokens -> Just (val, tokens)

  (<*>) = ap

instance Monad Parser where
  Parser parse >>= f = Parser $ \tokens -> do
    (value, rest) <- parse tokens
    let Parser f' = f value
    f' rest

instance Semigroup (Parser a) where
  Parser left <> Parser right = Parser $ \tokens -> case left tokens of
    Just (val, rest) -> Just (val, rest)
    Nothing -> right tokens

instance Monoid (Parser a) where
  mempty = Parser $ const Nothing

parseEnd :: Parser ()
parseEnd = Parser $ \case
  [] -> Just ((), [])
  _ -> Nothing

parseSingleToken :: (Lexer.Token -> Maybe a) -> Parser a
parseSingleToken transform = Parser $ \tokens -> do
  next <- listToMaybe tokens
  fmap (,tail tokens) (transform next)

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither (Parser left) (Parser right) = Parser $ \tokens -> case left tokens of
  Just (val, rest) -> Just (Left val, rest)
  Nothing -> do
    (val, rest) <- right tokens
    return (Right val, rest)

parseEmptyToken :: Lexer.Token -> Parser ()
parseEmptyToken token = parseSingleToken $ \t -> if t == token then Just () else Nothing

parseValue :: Parser Value
parseValue =
  -- TODO: Evaluate expressions
  mconcat
    [ parseSingleToken $ \case
        Lexer.Literal (Lexer.Number num) -> Just (Number num)
        Lexer.Literal (Lexer.String str) -> Just (String str)
        _ -> Nothing,
      do
        parseEmptyToken Lexer.ArrayBegin
        fmap Array parseArray,
      do
        parseEmptyToken Lexer.DictBegin
        fmap Dict parseDict
    ]

parseArray :: Parser [Value]
parseArray = do
  endOrValue <- parseEither (parseEmptyToken Lexer.ArrayEnd) parseValue

  case endOrValue of
    Left _ -> return []
    Right value -> do
      endOrDelim <- parseEither (parseEmptyToken Lexer.ArrayEnd) (parseEmptyToken Lexer.Delimiter)

      case endOrDelim of
        Left _ -> return [value]
        Right _ -> do
          rest <- parseArray
          return $ value : rest

parseDict :: Parser [(String, Value)]
parseDict = do
  endOrKey <-
    parseEither
      (parseEmptyToken Lexer.DictEnd)
      ( parseSingleToken $ \case
          Lexer.Identifier key -> Just key
          _ -> Nothing
      )

  case endOrKey of
    Left _ -> return []
    Right key -> do
      parseEmptyToken Lexer.DictSep
      value <- parseValue
      endOrDelim <- parseEither (parseEmptyToken Lexer.DictEnd) (parseEmptyToken Lexer.Delimiter)

      case endOrDelim of
        Left _ -> return [(key, value)]
        Right _ -> do
          rest <- parseDict
          return $ (key, value) : rest

parseEntry :: Parser ConfigEntry
parseEntry =
  mconcat
    [ parseSingleToken $ \case
        Lexer.Comment comment -> Just (Comment comment)
        _ -> Nothing,
      do
        parseEmptyToken Lexer.Set

        key <- parseSingleToken $ \case
          Lexer.Identifier key -> Just key
          _ -> Nothing

        parseEmptyToken Lexer.Equals
        value <- parseValue

        return $ Set (key, value)
    ]

parseConfig :: Parser Config
parseConfig = do
  endOrEntry <- parseEither parseEnd parseEntry

  case endOrEntry of
    Left _ -> return []
    Right entry -> do
      rest <- parseConfig
      return $ entry : rest
