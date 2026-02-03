module Proto.SseStatus
  ( SseStatus(..)
  , sseStatusToString
  , sseStatusFromString
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (toLower)

data SseStatus
  = Success
  | Duplicated
  | Failed

derive instance eqSseStatus :: Eq SseStatus

instance showSseStatus :: Show SseStatus where
  show = case _ of
    Success -> "Success"
    Duplicated -> "Duplicated"
    Failed -> "Failed"

sseStatusToString :: SseStatus -> String
sseStatusToString = case _ of
  Success -> "success"
  Duplicated -> "duplicated"
  Failed -> "failed"

sseStatusFromString :: String -> Maybe SseStatus
sseStatusFromString s =
  case toLower s of
    "success" -> Just Success
    "duplicated" -> Just Duplicated
    "duplicate" -> Just Duplicated
    "failed" -> Just Failed
    _ -> Nothing

instance encodeJsonSseStatus :: EncodeJson SseStatus where
  encodeJson = encodeJson <<< sseStatusToString

instance decodeJsonSseStatus :: DecodeJson SseStatus where
  decodeJson json = do
    s <- decodeJson json
    case sseStatusFromString s of
      Just v -> Right v
      Nothing -> Left (TypeMismatch "SseStatus")
