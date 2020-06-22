{-# language OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>), many)
import Data.Bifunctor (first)
import qualified Data.Text.Lazy as Lazy
import Text.Sage (parse, between, char, sepBy, eof)
import Text.Diagnostic (Config(colors), render, defaultConfig)
import Text.Diagnostic.Sage (parseError)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "examples" $ do
      describe "sexpr" $ do
        let
          spaces = many (char ' ')

          sexpr =
            between (char '(') (char ')') (concat <$> sepBy sexpr spaces) <|>
            pure <$> char 'x'
        it "(x x" $ do
          let
            input = "(x x"

            actual =
              first (render (defaultConfig { colors = Nothing }) "test" input . parseError) $
              parse (sexpr <* eof) input
            expected =
              Left $
              Lazy.unlines
              [ "test:1:5: error: expected one of: ' ', '(', ')', 'x'"
              , "  |"
              , "1 | (x x"
              , "  |     ^"
              ]
          actual `shouldBe` expected
        it "xy" $ do
          let
            input = "xy"

            actual =
              first (render (defaultConfig { colors = Nothing }) "test" input . parseError) $
              parse (sexpr <* eof) input
            expected =
              Left $
              Lazy.unlines
              [ "test:1:2: error: expected end of file"
              , "  |"
              , "1 | xy"
              , "  |  ^"
              ]
          actual `shouldBe` expected
