{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (many, (<|>))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteString.Char8
import Test.Hspec
import Text.Diagnostic (Config (colors), defaultConfig, render)
import Text.Diagnostic.Sage (parseError)
import Text.Parser.Char (char)
import Text.Parser.Combinators (between, eof, sepBy)
import Text.Parser.Sage.Instances ()
import Text.Sage (parse)

main :: IO ()
main =
  hspec $ do
    describe "examples" $ do
      describe "sexpr" $ do
        let
          spaces = many (char ' ')

          sexpr =
            between (char '(') (char ')') (concat <$> sepBy sexpr spaces)
              <|> pure <$> char 'x'
        it "(x x" $ do
          let
            input = "(x x"

            actual =
              first (render (defaultConfig{colors = Nothing}) "test" input . parseError) $
                parse (sexpr <* eof) (LazyByteString.toStrict input)
            expected =
              Left $
                LazyByteString.Char8.unlines
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
              first (render (defaultConfig{colors = Nothing}) "test" input . parseError) $
                parse (sexpr <* eof) (LazyByteString.toStrict input)
            expected =
              Left $
                LazyByteString.Char8.unlines
                  [ "test:1:2: error: expected end of file"
                  , "  |"
                  , "1 | xy"
                  , "  |  ^"
                  ]
          actual `shouldBe` expected
