{-# language OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>), many, some)
import System.Environment (getArgs)
import Text.Diagnostic (render, defaultConfig)
import Text.Diagnostic.Sage (parseError)
import Text.Sage (Parser, parseText)
import Text.Parser.Combinators (between, eof, (<?>))
import Text.Parser.Char (char, lower, string)
import Data.Text (Text)
import Streaming.Chars (Chars)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Lazy

data Expr = Var Text | Lam Text Expr | App Expr Expr
  deriving Show

expr :: Chars s => Parser s Expr
expr =
  app <|>
  lam
  where
    spaces = many (char ' ') <?> "spaces"
    ident = fmap Text.pack (some lower) <?> "identifier"
    lam =
      Lam <$ char '\\' <* spaces <*>
      ident <* spaces <* string "->" <* spaces <*>
      expr
    app = foldl App <$> atom <*> many atom
    atom =
      Var <$> ident <* spaces <|>
      between (char '(' <* spaces) (char ')' <* spaces) expr

main :: IO ()
main = do
  rawInput:_ <- getArgs
  let input = Text.pack rawInput
  case parseText (expr <* eof) input of
    Left err -> Lazy.putStrLn (render defaultConfig "test" input $ parseError err)
    Right res -> print res
