{-# LANGUAGE OverloadedStrings #-}
module Tests.Unit.Proto.SymbolsSpec (tests) where

import qualified Data.Aeson as A
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Prelude
import Proto.Symbols (APISymbolsResponse (..), SymbolInfo (..))
import Test.Hspec

expectObjectHasKeys :: [Key] -> LBS.ByteString -> Expectation
expectObjectHasKeys keys body =
  case A.decode body of
    Just (A.Object obj) ->
      mapM_ (\k -> KM.member k obj `shouldBe` True) keys
    _ -> expectationFailure ("expected JSON object, got: " <> show body)

tests :: Spec
tests = do
  describe "Proto.Symbols" do
    it "SymbolInfo ToJSON/FromJSON roundtrip" do
      let s = SymbolInfo
            { symbolType = "stock"
            , code = "AAPL"
            , name = "Apple"
            }
      A.decode (A.encode s) `shouldBe` Just s

    it "APISymbolsResponse JSON keys" do
      let payload = APISymbolsResponse
            { complete = True
            , symbols = []
            }
      expectObjectHasKeys ["complete", "symbols"] (A.encode payload)
