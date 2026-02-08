{-# LANGUAGE OverloadedStrings #-}
module Tests.Unit.Web.Service.ColdWarm.CoveragePolicySpec (tests) where

import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import Prelude
import Test.Hspec
import Web.Service.ColdWarm.CandlesCtx (CandlesCtx (..))
import Web.Service.ColdWarm.Policy.CoveragePolicy (coveredByRequest)
import Web.Service.ColdWarm.SymbolsCtx (SymbolsCtx (..))
import Web.Types (SymbolType (..))

mkTime :: Integer -> Int -> Int -> LocalTime
mkTime y m d = LocalTime (fromGregorian y m d) (TimeOfDay 0 0 0)

tests :: Spec
tests = do
  describe "CoveragePolicy" do
    it "candles coveredByRequest true (same client, narrower window)" do
      let t1 = mkTime 2024 1 1
          t2 = mkTime 2024 1 31
          t3 = mkTime 2024 1 15
          ctxA = CandlesCtx
              { symbolType = Stock
              , symbolCode = "AAPL"
              , timeframe = 1
              , fromDt = t1
              , toDt = t2
              , clientId = "client-a"
              }
          ctxB = CandlesCtx
              { symbolType = Stock
              , symbolCode = "AAPL"
              , timeframe = 1
              , fromDt = t3
              , toDt = t3
              , clientId = "client-a"
              }
      coveredByRequest ctxA ctxB `shouldBe` True

    it "candles coveredByRequest false (different client)" do
      let t1 = mkTime 2024 1 1
          t2 = mkTime 2024 1 31
          t3 = mkTime 2024 1 15
          ctxA = CandlesCtx
              { symbolType = Stock
              , symbolCode = "AAPL"
              , timeframe = 1
              , fromDt = t1
              , toDt = t2
              , clientId = "client-a"
              }
          ctxC = CandlesCtx
              { symbolType = Stock
              , symbolCode = "AAPL"
              , timeframe = 1
              , fromDt = t3
              , toDt = t3
              , clientId = "client-b"
              }
      coveredByRequest ctxA ctxC `shouldBe` False

    it "symbols coveredByRequest true (same client)" do
      let s1 = SymbolsCtx { clientId = "client-a" }
          s2 = SymbolsCtx { clientId = "client-a" }
      coveredByRequest s1 s2 `shouldBe` True

    it "symbols coveredByRequest false (different client)" do
      let s1 = SymbolsCtx { clientId = "client-a" }
          s3 = SymbolsCtx { clientId = "client-b" }
      coveredByRequest s1 s3 `shouldBe` False
