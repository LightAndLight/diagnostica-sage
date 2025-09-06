{-# LANGUAGE OverloadedStrings #-}

module Text.Diagnostic.Sage (parseError)
where

import qualified Data.ByteString.Builder as Builder
import Data.Foldable (fold, toList)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Text.Diagnostic (Diagnostic (Caret), Message (..), Position (Offset), Report, emit)
import Text.Sage (Label (..), ParseError (..))

parseError :: ParseError -> Report
parseError (Unexpected pos expecteds) =
  emit (Offset pos) Caret (Message mkMessage)
  where
    renderLabel l =
      case l of
        String s -> Builder.byteString . Text.Encoding.encodeUtf8 $ Text.pack s
        Text t -> Builder.byteString $ Text.Encoding.encodeUtf8 t
        Char c -> Builder.byteString . Text.Encoding.encodeUtf8 . Text.pack $ show c
        Eof -> Builder.byteString "end of file"

    expectedsList = toList expecteds

    mkMessage =
      Builder.byteString
        ( case expectedsList of
            [_] -> "expected "
            _ -> "expected one of: "
        )
        <> fold
          ( List.intersperse
              (Builder.byteString ", ")
              (renderLabel <$> expectedsList)
          )
