module Xml (XmlConfig (..), showConfig) where

import Parser (Config, Value (..))
import Text.XML.Light (Node (..), ppElement, unode)

newtype XmlValue = XmlValue Value

newtype XmlConfig = XmlConfig Config

instance Node XmlValue where
  node qn (XmlValue (Number num)) = node qn $ show num
  node qn (XmlValue (String str)) = node qn str
  node qn (XmlValue (Array entries)) = node qn $ map (unode "entry" . XmlValue) entries
  node qn (XmlValue (Dict entries)) = node qn $ map (\(key, val) -> unode key (XmlValue val)) entries

instance Node XmlConfig where
  node qn (XmlConfig entries) = node qn $ map (\(key, value) -> unode key $ XmlValue value) entries

showConfig :: Config -> String
showConfig = ppElement . unode "config" . XmlConfig
