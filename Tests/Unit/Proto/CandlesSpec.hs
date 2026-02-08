{-# LANGUAGE OverloadedStrings #-}
module Tests.Unit.Proto.CandlesSpec (tests) where

import qualified Data.Aeson as A
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Prelude
import Proto.Candles (Candle (..), CandlesResponse (..))
import Test.Hspec

expectObjectHasKeys :: [Key] -> LBS.ByteString -> Expectation
expectObjectHasKeys keys body =
  case A.decode body of
    Just (A.Object obj) ->
      mapM_ (\k -> KM.member k obj `shouldBe` True) keys
    _ -> expectationFailure ("expected JSON object, got: " <> show body)

tests :: Spec
tests = do
  describe "Proto.Candles" do
    it "Candle ToJSON/FromJSON roundtrip" do
      let c = Candle
            { time = "2024-01-01T00:00:00"
            , open = 1
            , high = 2
            , low = 1
            , close = 2
            , volume = Just 10
            , amount = Nothing
            }
      A.decode (A.encode c) `shouldBe` Just c

    it "CandlesResponse JSON keys" do
      let payload = CandlesResponse
            { complete = True
            , symbolType = "stock"
            , symbolCode = "AAPL"
            , timeframe = 1
            , dataPoints = []
            }
      expectObjectHasKeys ["complete", "symbolType", "symbolCode", "timeframe", "dataPoints"] (A.encode payload)
