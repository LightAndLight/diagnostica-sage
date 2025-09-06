{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (many, some, (<|>))
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import System.Environment (getArgs)
import Text.Diagnostic (defaultConfig, render)
import Text.Diagnostic.Sage (parseError)
import Text.Parser.Char (char, lower, string)
import Text.Parser.Combinators (between, eof, (<?>))
import Text.Parser.Sage.Instances ()
import Text.Sage (Parser, parse)

data Expr = Var Text | Lam Text Expr | App Expr Expr
  deriving (Show)

expr :: Parser Expr
expr =
  app
    <|> lam
  where
    spaces = many (char ' ') <?> "spaces"
    ident = fmap Text.pack (some lower) <?> "identifier"
    lam =
      Lam
        <$ char '\\'
        <* spaces
        <*> ident
        <* spaces
        <* string "->"
        <* spaces
        <*> expr
    app = foldl App <$> atom <*> many atom
    atom =
      Var <$> ident <* spaces
        <|> between (char '(' <* spaces) (char ')' <* spaces) expr

main :: IO ()
main = do
  rawInput : _ <- getArgs
  let input = Text.Lazy.Encoding.encodeUtf8 $ Text.Lazy.pack rawInput
  case parse (expr <* eof) (ByteString.Lazy.toStrict input) of
    Left err -> ByteString.Lazy.Char8.putStrLn (render defaultConfig "test" input $ parseError err)
    Right res -> print res
