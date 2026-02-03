{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Proto.SseStatus
  ( SseStatus(..)
  , sseStatusToText
  , sseStatusFromText
  ) where

import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, withObject, withText, (.:), (.:?), (.=))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Prelude

data SseStatus
  = Success
  | Duplicated
  | Failed Text
  deriving (Show, Eq, Generic)

sseStatusToText :: SseStatus -> Text
sseStatusToText = \case
  Success    -> "success"
  Duplicated -> "duplicated"
  Failed _   -> "failed"

sseStatusFromText :: Text -> Maybe SseStatus
sseStatusFromText t =
  case T.toLower t of
    "success"    -> Just Success
    "duplicated" -> Just Duplicated
    "failed"     -> Just (Failed "")
    _            -> Nothing

instance ToJSON SseStatus where
  toJSON status =
    case status of
      Success -> object [ "status" .= ("success" :: Text) ]
      Duplicated -> object [ "status" .= ("duplicated" :: Text) ]
      Failed reason -> object [ "status" .= ("failed" :: Text), "reason" .= reason ]

instance FromJSON SseStatus where
  parseJSON = withObject "SseStatus" \obj -> do
    status <- obj .: "status"
    reason <- obj .:? "reason"
    case (sseStatusFromText status, reason) of
      (Just (Failed _), Just r) -> pure (Failed r)
      (Just v, _) -> pure v
      _ -> fail ("unknown SseStatus: " <> T.unpack status)
