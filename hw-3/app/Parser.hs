{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser
  ( Value (..),
    ParsingErrorDetails (..),
    ParsingError (..),
    ConfigEntry (..),
    Config,
    parseConfig,
  )
where

import Control.Monad (ap, liftM)
import Data.Maybe (listToMaybe)
import qualified Lexer
import Util (maybeToEither)

data Value
  = Array [Value]
  | Dict [(String, Value)]
  | Number Integer
  | String String
  deriving (Show, Eq)

data ParsingErrorDetails
  = UnexpectedToken String Lexer.Token
  | UnexpectedEnd
  deriving (Show)

data ParsingError = ParsingError {pos :: Integer, details :: ParsingErrorDetails} deriving (Show)

newtype Parser a = Parser ([Lexer.Token] -> Either ParsingError (a, [Lexer.Token]))

data ConfigEntry
  = Comment String
  | Set (String, Value)
  deriving (Show, Eq)

type Config = [ConfigEntry]

addErrPos :: Integer -> ParsingError -> ParsingError
addErrPos delta err = ParsingError (pos err + delta) $ details err

instance Semigroup ParsingError where
  left <> right =
    if pos left <= pos right
      then right
      else left

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure val = Parser $ \tokens -> Right (val, tokens)

  (<*>) = ap

instance Monad Parser where
  Parser parse >>= f = Parser $ \tokens -> do
    (value, rest) <- parse tokens
    let Parser f' = f value
    case f' rest of
      Right res -> Right res
      Left err -> Left $ addErrPos 1 err

instance Semigroup (Parser a) where
  Parser left <> Parser right = Parser $ \tokens -> case left tokens of
    Right res -> Right res
    Left lerr -> case right tokens of
      Right res -> Right res
      Left rerr -> Left $ lerr <> rerr

parseEnd :: Parser ()
parseEnd = Parser $ \case
  [] -> Right ((), [])
  tok : _ -> Left $ ParsingError 0 $ UnexpectedToken "expected no token" tok

parseSingleToken :: (Lexer.Token -> Either ParsingError a) -> Parser a
parseSingleToken transform = Parser $ \tokens -> do
  next <- maybeToEither (ParsingError 0 UnexpectedEnd) $ listToMaybe tokens
  fmap (,tail tokens) (transform next)

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither (Parser left) (Parser right) = Parser $ \tokens -> case left tokens of
  Right (val, rest) -> Right (Left val, rest)
  Left err -> case right tokens of
    Right (val, rest) -> Right (Right val, rest)
    Left err' -> Left $ err <> err'

parseEmptyToken :: Lexer.Token -> Parser ()
parseEmptyToken token = parseSingleToken $ \t ->
  if t == token
    then Right ()
    else Left (ParsingError 0 $ UnexpectedToken ("expected token " <> show token) t)

parseValue :: Parser Value
parseValue =
  -- TODO: Evaluate expressions
  parseSingleToken
    ( \case
        Lexer.Literal (Lexer.Number num) -> Right (Number num)
        Lexer.Literal (Lexer.String str) -> Right (String str)
        tok -> Left $ ParsingError 0 $ UnexpectedToken "expected string or number literal" tok
    )
    <> ( do
           parseEmptyToken Lexer.ArrayBegin
           fmap Array parseArray
       )
    <> ( do
           parseEmptyToken Lexer.DictBegin
           fmap Dict parseDict
       )

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
          Lexer.Identifier key -> Right key
          tok -> Left $ ParsingError 0 $ UnexpectedToken "expected identifier" tok
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
  parseSingleToken
    ( \case
        Lexer.Comment comment -> Right (Comment comment)
        tok -> Left $ ParsingError 0 $ UnexpectedToken "expected comment" tok
    )
    <> ( do
           parseEmptyToken Lexer.Set

           key <- parseSingleToken $ \case
             Lexer.Identifier key -> Right key
             tok -> Left $ ParsingError 0 $ UnexpectedToken "expected identifier" tok

           parseEmptyToken Lexer.Equals
           value <- parseValue

           return $ Set (key, value)
       )

parseConfig' :: Parser Config
parseConfig' = do
  endOrEntry <- parseEither parseEnd parseEntry

  case endOrEntry of
    Left _ -> return []
    Right entry -> do
      rest <- parseConfig'
      return $ entry : rest

parseConfig :: [Lexer.Token] -> Either ParsingError Config
parseConfig tokens =
  let Parser parse = parseConfig'
      res = parse tokens
   in fst <$> res
