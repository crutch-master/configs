module Util (flattenEither, maybeToEither, splitOnce) where

import Data.List (stripPrefix)

flattenEither :: Either a a -> a
flattenEither e = case e of
  Left val -> val
  Right val -> val

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err mb = case mb of
  Just val -> Right val
  _ -> Left err

splitOnce :: String -> String -> Maybe (String, String)
splitOnce delim text = case stripPrefix delim text of
  Just rest -> Just ("", rest)
  Nothing -> case text of
    "" -> Nothing
    (x : rest) -> do
      (left, right) <- splitOnce delim rest
      return (x : left, right)