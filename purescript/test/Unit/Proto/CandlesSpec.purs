module Test.Unit.Proto.CandlesSpec (tests) where

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude
import Proto.Candles (Candle(..), CandlesResponse(..))
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "roundtrips Candle via JSON" do
    let c =
          Candle
            { time: "2024-01-01T00:00:00"
            , open: 1.0
            , high: 2.0
            , low: 1.0
            , close: 2.0
            , volume: Just 10.0
            , amount: Nothing
            }
    case decodeJson (encodeJson c) of
      Right (Candle r) -> do
        r.time `shouldEqual` "2024-01-01T00:00:00"
        r.open `shouldEqual` 1.0
        r.high `shouldEqual` 2.0
        r.low `shouldEqual` 1.0
        r.close `shouldEqual` 2.0
        r.volume `shouldEqual` Just 10.0
        r.amount `shouldEqual` Nothing
      Left _ -> shouldEqual true false

  it "roundtrips CandlesResponse via JSON" do
    let payload =
          CandlesResponse
            { complete: true
            , symbolType: "stock"
            , symbolCode: "AAPL"
            , timeframe: 1
            , dataPoints: []
            }
    case decodeJson (encodeJson payload) of
      Right (CandlesResponse r) -> do
        r.complete `shouldEqual` true
        r.symbolType `shouldEqual` "stock"
        r.symbolCode `shouldEqual` "AAPL"
        r.timeframe `shouldEqual` 1
        length r.dataPoints `shouldEqual` 0
      Left _ -> shouldEqual true false
