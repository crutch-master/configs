module Main where

import Lexer (LexingError, tokenize)
import Parser (Config (..), ParsingError, Value (..), parseConfig)
import Test.Hspec (describe, hspec, it, shouldBe)

tokenizeAndParse :: String -> Either (Either LexingError ParsingError) Config
tokenizeAndParse text = case tokenize text of
  Left err -> Left $ Left err
  Right tokens -> case parseConfig tokens of
    Left err -> Left $ Right err
    Right result -> Right result

main :: IO ()
main = hspec $ do
  describe "parsing" $ do
    it "should simple config" $ do
      tokenizeAndParse "set a = 1" `shouldBe` Right (Config [("a", Number 1)] [])

    it "should evaluate expressions" $ do
      tokenizeAndParse "set val = ?[2 1 + 3 * print]" `shouldBe` Right (Config [("val", Number 9)] ["Number 9"])

    it "should parse arrays and dictionaries" $
      do
        tokenizeAndParse
          "\
          \set dict = { val: \"str\", nested: { val2: \"str2\" } }\n\
          \set arr = array(1, \"str\", array(dict))\n\
          \"
        `shouldBe` Right
          ( Config
              [ ( "dict",
                  Dict
                    [ ("val", String "str"),
                      ("nested", Dict [("val2", String "str2")])
                    ]
                ),
                ( "arr",
                  Array
                    [ Number 1,
                      String "str",
                      Array
                        [ Dict
                            [ ("val", String "str"),
                              ("nested", Dict [("val2", String "str2")])
                            ]
                        ]
                    ]
                )
              ]
              []
          )

    it "should skip comments" $ do
      tokenizeAndParse "#= this should not be included =# set val = ?[\"asdf\" print]"
        `shouldBe` Right
          ( Config
              [("val", String "asdf")]
              ["String \"asdf\""]
          )