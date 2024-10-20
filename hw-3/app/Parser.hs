{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser
  ( Value (..),
    ParsingErrorDetails (..),
    ParsingError (..),
    Config,
    parseConfig,
  )
where

import Control.Monad (ap, liftM)
import Data.Maybe (listToMaybe)
import qualified Lexer
import Text.Regex.TDFA.Common (snd3)
import Util (maybeToEither)

data Value
  = Array [Value]
  | Dict [(String, Value)]
  | Number Integer
  | String String
  deriving (Show, Eq)

data Operation
  = Add
  | Subtract
  | Multiply
  | Print
  deriving (Show, Eq)

data ParsingErrorDetails
  = UnexpectedToken String Lexer.Token
  | UnexpectedEnd
  | InvalidExpression
  | KeyNotFound String
  deriving (Show)

data ParsingError = ParsingError {pos :: Integer, details :: ParsingErrorDetails} deriving (Show)

type Config = [(String, Value)]

newtype Parser a = Parser (Config -> [Lexer.Token] -> Either ParsingError (a, Config, [Lexer.Token]))

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
  pure val = Parser $ \entries tokens -> Right (val, entries, tokens)

  (<*>) = ap

instance Monad Parser where
  Parser parse >>= f = Parser $ \entries tokens -> do
    (value, entries', rest) <- parse entries tokens
    let Parser f' = f value
    case f' entries' rest of
      Right res -> Right res
      Left err -> Left $ addErrPos 1 err

instance Semigroup (Parser a) where
  Parser left <> Parser right = Parser $ \entries tokens -> case left entries tokens of
    Right res -> Right res
    Left lerr -> case right entries tokens of
      Right res -> Right res
      Left rerr -> Left $ lerr <> rerr

parseEnd :: Parser ()
parseEnd = Parser $ \entries -> \case
  [] -> Right ((), entries, [])
  tok : _ -> Left . ParsingError 0 $ UnexpectedToken "expected no token" tok

write :: String -> Value -> Parser ()
write key value = Parser $ \config tokens -> Right ((), (key, value) : config, tokens)

lookupValue :: String -> Parser Value
lookupValue key = Parser $ \config tokens -> case lookup key config of
  Just value -> Right (value, config, tokens)
  Nothing -> Left . ParsingError 0 $ KeyNotFound key

parseSingleToken :: (Lexer.Token -> Either ParsingError a) -> Parser a
parseSingleToken transform = Parser $ \entries tokens -> do
  next <- maybeToEither (ParsingError 0 UnexpectedEnd) $ listToMaybe tokens
  fmap (,entries,tail tokens) (transform next)

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither (Parser left) (Parser right) = Parser $ \entries tokens -> case left entries tokens of
  Right (val, entries', rest) -> Right (Left val, entries', rest)
  Left err -> case right entries tokens of
    Right (val, entries', rest) -> Right (Right val, entries', rest)
    Left err' -> Left $ err <> err'

parseEmptyToken :: Lexer.Token -> Parser ()
parseEmptyToken token = parseSingleToken $ \t ->
  if t == token
    then Right ()
    else Left . ParsingError 0 $ UnexpectedToken ("expected token " <> show token) t

parseValue :: Parser Value
parseValue =
  parseSingleToken
    ( \case
        Lexer.Literal (Lexer.Number num) -> Right (Number num)
        Lexer.Literal (Lexer.String str) -> Right (String str)
        tok -> Left . ParsingError 0 $ UnexpectedToken "expected string or number literal" tok
    )
    <> ( do
           key <- parseSingleToken $ \case
             Lexer.Identifier key -> Right key
             tok -> Left . ParsingError 0 $ UnexpectedToken "expected identifier" tok
           lookupValue key
       )
    <> ( do
           parseEmptyToken Lexer.ArrayBegin
           fmap Array parseArray
       )
    <> ( do
           parseEmptyToken Lexer.DictBegin
           fmap Dict parseDict
       )
    <> ( do
           parseEmptyToken Lexer.ExprBegin
           parseExpression []
       )

parseOperation :: Parser Operation
parseOperation =
  parseSingleToken
    ( \case
        Lexer.Add -> Right Add
        Lexer.Subtract -> Right Subtract
        Lexer.Multiply -> Right Multiply
        Lexer.Print -> Right Print
        tok -> Left . ParsingError 0 $ UnexpectedToken "expected operation" tok
    )

performOperation :: Operation -> [Value] -> Parser [Value]
performOperation Print (top : rest) = return (top : rest)
performOperation op (right : left : rest) =
  case (left, right) of
    (Number l, Number r) -> case op of
      Add -> return $ Number (l + r) : rest
      Subtract -> return $ Number (l - r) : rest
      Multiply -> return $ Number (l * r) : rest
    _ -> Parser . const . const . Left $ ParsingError 0 InvalidExpression
performOperation _ _ = Parser . const . const . Left $ ParsingError 0 InvalidExpression

parseExpression :: [Value] -> Parser Value
parseExpression stack = do
  endOrValueOrOperation <- parseEither (parseEmptyToken Lexer.ExprEnd) $ parseEither parseValue parseOperation

  case endOrValueOrOperation of
    Left _ -> case stack of
      [top] -> return top
      _ -> Parser . const . const . Left $ ParsingError 0 InvalidExpression
    Right valOrOperation -> case valOrOperation of
      Left val -> parseExpression $ val : stack
      Right operation -> do
        stack' <- performOperation operation stack
        parseExpression stack'

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
          tok -> Left . ParsingError 0 $ UnexpectedToken "expected identifier" tok
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

parseEntry :: Parser ()
parseEntry = do
  parseEmptyToken Lexer.Set

  key <- parseSingleToken $ \case
    Lexer.Identifier key -> Right key
    tok -> Left . ParsingError 0 $ UnexpectedToken "expected identifier" tok

  parseEmptyToken Lexer.Equals
  value <- parseValue
  write key value

parseConfig' :: Parser ()
parseConfig' = do
  endOrEntry <- parseEither parseEnd parseEntry

  case endOrEntry of
    Left _ -> return ()
    Right _ -> do parseConfig'

parseConfig :: [Lexer.Token] -> Either ParsingError Config
parseConfig tokens =
  let Parser parse = parseConfig'
      res = parse [] tokens
   in fmap (reverse . snd3) res
