{-# LANGUAGE OverloadedStrings #-}
module Tests.Web.Service.SymbolsCtxSpec (tests) where

import qualified Data.Aeson as A
import Proto.SseStatus (SseStatus (..))
import qualified Proto.Symbols as Proto
import Prelude
import Test.Hspec
import Web.Service.Policy.RespondPolicy (respondHttp, respondSse)
import Web.Service.Policy.TTLPolicy (ttlKey, ttlSeconds)
import Web.Service.SymbolsCtx (SymbolsCtx (..))

tests :: Spec
tests = do
  describe "SymbolsCtx RespondPolicy" do
    it "respondHttp renders expected fields" do
      let ctx = SymbolsCtx { clientId = "client-a" }
          payload = Proto.APISymbolsResponse { complete = True, symbols = [] }
      respondHttp ctx payload `shouldBe`
        A.object [ "symbols" A..= ([] :: [Proto.SymbolInfo]) ]

    it "respondSse success/failed" do
      let ctx = SymbolsCtx { clientId = "client-a" }
      respondSse ctx Success `shouldBe` A.object [ "status" A..= ("success" :: String) ]
      respondSse ctx Failed `shouldBe` A.object [ "status" A..= ("failed" :: String) ]

  describe "SymbolsCtx TTLPolicy" do
    it "ttlKey and ttlSeconds" do
      let ctx = SymbolsCtx { clientId = "client-a" }
      ttlKey ctx `shouldBe` "symbols"
      ttlSeconds ctx `shouldBe` 604800
