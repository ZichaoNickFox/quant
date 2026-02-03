module Test.Proto.SymbolsSpec (tests) where

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (length)
import Data.Either (Either(..))
import Prelude
import Proto.Symbols (APISymbolsResponse(..), SymbolInfo(..))
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "roundtrips SymbolInfo via JSON" do
    let s =
          SymbolInfo
            { symbolType: "stock"
            , code: "AAPL"
            , name: "Apple"
            }
    case decodeJson (encodeJson s) of
      Right (SymbolInfo r) -> do
        r.symbolType `shouldEqual` "stock"
        r.code `shouldEqual` "AAPL"
        r.name `shouldEqual` "Apple"
      Left _ -> shouldEqual true false

  it "roundtrips APISymbolsResponse via JSON" do
    let payload =
          APISymbolsResponse
            { complete: true
            , symbols: []
            }
    case decodeJson (encodeJson payload) of
      Right (APISymbolsResponse r) -> do
        r.complete `shouldEqual` true
        length r.symbols `shouldEqual` 0
      Left _ -> shouldEqual true false
