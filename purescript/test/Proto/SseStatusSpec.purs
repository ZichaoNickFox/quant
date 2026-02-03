module Test.Proto.SseStatusSpec (tests) where

import Prelude

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Proto.SseStatus (SseStatus(..), sseStatusFromString, sseStatusToString)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "roundtrips SseStatus via JSON" do
    let encoded = encodeJson Success
    decodeJson encoded `shouldEqual` Right Success

  it "decodes case-insensitive strings" do
    decodeJson (encodeJson "SuCcEsS") `shouldEqual` Right Success
    decodeJson (encodeJson "DUPLICATED") `shouldEqual` Right Duplicated
    decodeJson (encodeJson "failed") `shouldEqual` Right Failed

  it "rejects unknown status strings" do
    let decoded :: Either JsonDecodeError SseStatus
        decoded = decodeJson (encodeJson "unknown")
    decoded `shouldEqual` Left (TypeMismatch "SseStatus")

  it "string helpers map both directions" do
    sseStatusToString Success `shouldEqual` "success"
    sseStatusFromString "duplicate" `shouldEqual` Just Duplicated
