{-# LANGUAGE OverloadedStrings #-}
module Tests.Unit.Proto.StrategySpec (tests) where

import qualified Data.Aeson as A
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Prelude
import Proto.Strategy (StrategyInfo (..))
import Test.Hspec

expectObjectHasKeys :: [Key] -> LBS.ByteString -> Expectation
expectObjectHasKeys keys body =
  case A.decode body of
    Just (A.Object obj) ->
      mapM_ (\k -> KM.member k obj `shouldBe` True) keys
    _ -> expectationFailure ("expected JSON object, got: " <> show body)

tests :: Spec
tests = do
  describe "Proto.Strategy" do
    it "StrategyInfo ToJSON/FromJSON roundtrip" do
      let s = StrategyInfo { id = "sid-1", name = "策略-1" }
      A.decode (A.encode s) `shouldBe` Just s

    it "StrategyInfo JSON keys" do
      let payload = StrategyInfo { id = "sid-1", name = "策略-1" }
      expectObjectHasKeys ["id", "name"] (A.encode payload)
