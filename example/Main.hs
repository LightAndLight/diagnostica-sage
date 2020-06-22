{-# language OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>), many)
import System.Environment (getArgs)
import Text.Diagnostic (render, defaultConfig)
import Text.Diagnostic.Sage (parseError)
import Text.Sage (Parser, (<?>), parse, between, char, takeWhile1, pLower, symbol, eof)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Lazy

data Expr = Var Text | Lam Text Expr | App Expr Expr
  deriving Show

expr :: Parser s Expr
expr =
  app <|>
  lam
  where
    spaces = many (char ' ') <?> "spaces"
    ident = takeWhile1 pLower <?> "identifier"
    lam =
      Lam <$ char '\\' <* spaces <*>
      ident <* spaces <* symbol "->" <* spaces <*>
      expr
    app = foldl App <$> atom <*> many atom
    atom =
      Var <$> ident <* spaces <|>
      between (char '(' <* spaces) (char ')' <* spaces) expr

main :: IO ()
main = do
  rawInput:_ <- getArgs
  let input = Text.pack rawInput
  case parse (expr <* eof) input of
    Left err -> Lazy.putStrLn (render defaultConfig "test" input $ parseError err)
    Right res -> print res
