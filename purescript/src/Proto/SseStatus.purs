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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower)

data SseStatus
  = Success
  | Duplicated
  | Failed String

derive instance eqSseStatus :: Eq SseStatus

instance showSseStatus :: Show SseStatus where
  show = case _ of
    Success -> "Success"
    Duplicated -> "Duplicated"
    Failed reason -> "Failed(" <> reason <> ")"

sseStatusToString :: SseStatus -> String
sseStatusToString = case _ of
  Success -> "success"
  Duplicated -> "duplicated"
  Failed _ -> "failed"

sseStatusFromString :: String -> Maybe SseStatus
sseStatusFromString s =
  case toLower s of
    "success" -> Just Success
    "duplicated" -> Just Duplicated
    "failed" -> Just (Failed "")
    _ -> Nothing

instance encodeJsonSseStatus :: EncodeJson SseStatus where
  encodeJson status =
    case status of
      Success -> encodeJson { status: "success" :: String, reason: (Nothing :: Maybe String) }
      Duplicated -> encodeJson { status: "duplicated" :: String, reason: (Nothing :: Maybe String) }
      Failed reason -> encodeJson { status: "failed" :: String, reason: Just reason }

instance decodeJsonSseStatus :: DecodeJson SseStatus where
  decodeJson json = do
    let
      fromObj = do
        o <- decodeJson json :: Either JsonDecodeError { status :: String, reason :: Maybe String }
        case sseStatusFromString o.status of
          Just (Failed _) -> Right (Failed (fromMaybe "" o.reason))
          Just v -> Right v
          Nothing -> Left (TypeMismatch "SseStatus")
    case decodeJson json of
      Right s ->
        case sseStatusFromString s of
          Just v -> Right v
          Nothing -> fromObj
      Left _ -> fromObj
