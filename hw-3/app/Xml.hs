module Xml (XmlConfig (..), toXmlConfig, showConfig) where

import Parser (Config, ConfigEntry (..), Value (..))
import Text.XML.Light (Node (..), ppElement, unode)

newtype XmlValue = XmlValue Value

newtype XmlConfig = XmlConfig [(String, Value)]

instance Node XmlValue where
  node qn (XmlValue (Number num)) = node qn $ show num
  node qn (XmlValue (String str)) = node qn str
  node qn (XmlValue (Array entries)) = node qn $ map (unode "entry" . XmlValue) entries
  node qn (XmlValue (Dict entries)) = node qn $ map (\(key, val) -> unode key (XmlValue val)) entries

instance Node XmlConfig where
  node qn (XmlConfig entries) = node qn $ map (\(key, value) -> unode key $ XmlValue value) entries

toXmlConfig :: Config -> XmlConfig
toXmlConfig =
  XmlConfig
    . foldr
      ( \entry rest -> case entry of
          Set pair -> pair : rest
          _ -> rest
      )
      []

showConfig :: XmlConfig -> String
showConfig = ppElement . unode "config"
