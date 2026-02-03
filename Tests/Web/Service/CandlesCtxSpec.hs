{-# LANGUAGE OverloadedStrings #-}
module Tests.Web.Service.CandlesCtxSpec (tests) where

import qualified Data.Aeson as A
import qualified Proto.Candles as Proto
import Proto.SseStatus (SseStatus (..))
import Prelude
import Test.Hspec
import Web.Service.CandlesCtx (CandlesCtx (..))
import Web.Service.Policy.RespondPolicy (respondHttp, respondSse)
import Web.Types (SymbolType (..))

tests :: Spec
tests = do
  describe "CandlesCtx RespondPolicy" do
    it "respondHttp renders expected fields" do
      let ctx = CandlesCtx
            { symbolType = Stock
            , symbolCode = "AAPL"
            , timeframe = 1
            , fromDt = read "2024-01-01 00:00:00"
            , toDt = read "2024-01-02 00:00:00"
            , clientId = "client-a"
            }
          payload = Proto.CandlesResponse True "stock" "AAPL" 1 []
      respondHttp ctx payload `shouldBe`
        A.object
          [ "symbolType" A..= ("stock" :: String)
          , "symbolCode" A..= ("AAPL" :: String)
          , "timeframe" A..= (1 :: Int)
          , "data" A..= ([] :: [Proto.Candle])
          ]

    it "respondSse success/failed" do
      let ctx = CandlesCtx
            { symbolType = Stock
            , symbolCode = "AAPL"
            , timeframe = 1
            , fromDt = read "2024-01-01 00:00:00"
            , toDt = read "2024-01-02 00:00:00"
            , clientId = "client-a"
            }
      respondSse ctx Success `shouldBe` A.object [ "status" A..= ("success" :: String) ]
      respondSse ctx Failed `shouldBe` A.object [ "status" A..= ("failed" :: String) ]
