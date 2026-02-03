{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Proto.SseStatus
  ( SseStatus(..)
  , sseStatusToText
  , sseStatusFromText
  ) where

import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withText)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Prelude

data SseStatus
  = Success
  | Duplicated
  | Failed
  deriving (Show, Eq, Generic)

sseStatusToText :: SseStatus -> Text
sseStatusToText = \case
  Success    -> "success"
  Duplicated -> "duplicated"
  Failed     -> "failed"

sseStatusFromText :: Text -> Maybe SseStatus
sseStatusFromText t =
  case T.toLower t of
    "success"    -> Just Success
    "duplicated" -> Just Duplicated
    "duplicate"  -> Just Duplicated
    "failed"     -> Just Failed
    _            -> Nothing

instance ToJSON SseStatus where
  toJSON = String . sseStatusToText

instance FromJSON SseStatus where
  parseJSON = withText "SseStatus" \t ->
    case sseStatusFromText t of
      Just v -> pure v
      Nothing -> fail ("unknown SseStatus: " <> T.unpack t)
