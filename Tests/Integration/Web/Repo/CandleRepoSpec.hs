{-# LANGUAGE OverloadedStrings #-}
module Tests.Integration.Web.Repo.CandleRepoSpec (tests) where

import Config (config)
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (withContext)
import Prelude
import Test.Hspec
import Web.FrontController ()
import Web.Prelude
import Web.Repo.CandleRepo (getCandlesWindow, hasCoverage, upsertCandles)
import Web.Types

mkTime :: Integer -> Int -> Int -> LocalTime
mkTime y m d = LocalTime (fromGregorian y m d) (TimeOfDay 0 0 0)

seedSymbolWithCandles :: (?modelContext :: ModelContext) => Text -> IO (UUID, SymbolType, Text, Int, LocalTime, LocalTime)
seedSymbolWithCandles code = do
  sym <-
    newRecord @Symbol
      |> set #code code
      |> set #symbolType Stock
      |> set #name "Apple"
      |> set #listDate (mkTime 2024 1 1)
      |> set #delistDate Nothing
      |> createRecord
  let (Id sid) = get #id sym
      tf = 1
      fromDt = mkTime 2024 1 1
      toDt = mkTime 2024 1 2
  _ <- createRecord $ newRecord @Candle
        |> set #symbolId sid
        |> set #timeframe tf
        |> set #datetime fromDt
        |> set #open 1
        |> set #close 1
        |> set #high 1
        |> set #low 1
        |> set #volume 1
        |> set #amount 1
  _ <- createRecord $ newRecord @Candle
        |> set #symbolId sid
        |> set #timeframe tf
        |> set #datetime toDt
        |> set #open 2
        |> set #close 2
        |> set #high 2
        |> set #low 2
        |> set #volume 2
        |> set #amount 2
  pure (sid, Stock, code, tf, fromDt, toDt)

seedSymbol :: (?modelContext :: ModelContext) => Text -> IO UUID
seedSymbol code = do
  sym <-
    newRecord @Symbol
      |> set #code code
      |> set #symbolType Stock
      |> set #name "Apple"
      |> set #listDate (mkTime 2024 1 1)
      |> set #delistDate Nothing
      |> createRecord
  let (Id sid) = get #id sym
  pure sid

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "CandleRepo" do
    it "getCandlesWindow returns inserted candles" $ withContext do
      (sid, _st, _code, tf, fromDt, toDt) <- seedSymbolWithCandles "AAPL1"
      cs <- getCandlesWindow sid tf fromDt toDt
      length cs `shouldBe` 2

    it "hasCoverage true when first and last exist" $ withContext do
      (_sid, st, code, tf, fromDt, toDt) <- seedSymbolWithCandles "AAPL2"
      ok <- hasCoverage st code tf fromDt toDt
      ok `shouldBe` True

    it "hasCoverage false when symbol missing" $ withContext do
      ok <- hasCoverage Stock "MISSING" 1 (mkTime 2024 1 1) (mkTime 2024 1 2)
      ok `shouldBe` False

    it "upsertCandles inserts and getCandlesWindow returns them" $ withContext do
      sid <- seedSymbol "AAPL3"
      let tf = 1
          t1 = mkTime 2024 1 1
          t2 = mkTime 2024 1 2
          c1 = newRecord @Candle
            |> set #symbolId sid
            |> set #timeframe tf
            |> set #datetime t1
            |> set #open 1
            |> set #close 1
            |> set #high 1
            |> set #low 1
            |> set #volume 1
            |> set #amount 1
          c2 = newRecord @Candle
            |> set #symbolId sid
            |> set #timeframe tf
            |> set #datetime t2
            |> set #open 2
            |> set #close 2
            |> set #high 2
            |> set #low 2
            |> set #volume 2
            |> set #amount 2
      upsertCandles [c1, c2]
      cs <- getCandlesWindow sid tf t1 t2
      length cs `shouldBe` 2
