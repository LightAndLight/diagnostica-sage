{-# language OverloadedStrings #-}
module Text.Diagnostic.Sage
  (parseError)
where

import Data.Foldable (fold, toList)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Builder
import Text.Sage (ParseError(..), Label(..))
import Text.Diagnostic (Position(Offset), Diagnostic(Caret), Report, emit)

parseError :: ParseError -> Report
parseError (Unexpected pos expecteds) =
  emit (Offset pos) Caret mkMessage
  where
    renderLabel l =
      case l of
        String s -> Text.pack s
        Text t -> t
        Char c -> Text.pack $! show c
        Eof -> "end of file"

    expectedsList = toList expecteds

    mkMessage =
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
