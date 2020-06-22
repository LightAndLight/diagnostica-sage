{-# language OverloadedStrings #-}
module Text.Diagnostic.Sage
  (parseError)
where

import Data.Foldable (fold, toList)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Text.Sage (ParseError(..), Label(..))
import Text.Diagnostic (Position(Offset), Diagnostic(Caret), Message(Message), Report, emit)

parseError :: ParseError -> Report
parseError (Unexpected pos expecteds) =
  emit (Offset pos) Caret (Message $! mkMessage)
  where
    renderLabel l =
      case l of
        Named n -> n
        Symbol s -> Text.cons '"' $! Text.snoc s '"'
        Char c -> Text.pack $! show c
        Eof -> "end of file"

    expectedsList = toList expecteds

    mkMessage =
      Lazy.toStrict . Builder.toLazyText $
      Builder.fromText
        (case expectedsList of
           [_] -> "expected "
           _ -> "expected one of: "
        ) <>
      fold
        (List.intersperse
          (Builder.fromText ", ")
          (Builder.fromText . renderLabel <$> expectedsList)
        )
