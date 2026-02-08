{-# LANGUAGE OverloadedStrings #-}
module Tests.Integration.Web.Repo.SymbolRepoSpec (tests) where

import Config (config)
import qualified Data.Map as M
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import qualified Generated.ActualTypes as Actual
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (withContext)
import Prelude
import Test.Hspec
import Web.FrontController ()
import Web.Prelude
import Web.Repo.SymbolRepo (clampCandleWindow, getSymbolsByTypeMap, upsertSymbols)
import Web.Types (SymbolType (..), WebApplication (..))

mkTime :: Integer -> Int -> Int -> LocalTime
mkTime y m d = LocalTime (fromGregorian y m d) (TimeOfDay 0 0 0)

mkSymbol :: LocalTime -> Maybe LocalTime -> Actual.Symbol
mkSymbol listDate delistDate =
  newRecord @Actual.Symbol
    |> set #listDate listDate
    |> set #delistDate delistDate

tests :: Spec
tests = do
  describe "SymbolRepo.clampCandleWindow" do
    it "clamps lower bound to listDate" do
      let sym = mkSymbol (mkTime 2024 1 10) Nothing
          (from', to') = clampCandleWindow sym (mkTime 2024 1 1) (mkTime 2024 2 1)
      from' `shouldBe` mkTime 2024 1 10
      to' `shouldBe` mkTime 2024 2 1

    it "clamps upper bound to delistDate when present" do
      let sym = mkSymbol (mkTime 2024 1 1) (Just (mkTime 2024 1 20))
          (from', to') = clampCandleWindow sym (mkTime 2024 1 5) (mkTime 2024 2 1)
      from' `shouldBe` mkTime 2024 1 5
      to' `shouldBe` mkTime 2024 1 20

  aroundAll (withIHPApp WebApplication config) do
    describe "SymbolRepo (integration)" do
      it "upsertSymbols inserts and getSymbolsByTypeMap returns them" $ withContext do
        let s1 = mkSymbol (mkTime 2024 1 1) Nothing
              |> set #code "AAA1"
              |> set #symbolType Stock
              |> set #name "AAA1"
            s2 = mkSymbol (mkTime 2024 1 2) Nothing
              |> set #code "BBB1"
              |> set #symbolType Stock
              |> set #name "BBB1"
        upsertSymbols [s1, s2]
        m <- getSymbolsByTypeMap
        let xs = fromMaybe [] (M.lookup Stock m)
        let codes = map (\(Actual.Symbol { code }) -> code) xs :: [Text]
        length (filter (`elem` ["AAA1", "BBB1"]) codes) `shouldBe` 2

    it "upsertSymbols ignores duplicates" $ withContext do
      let s = mkSymbol (mkTime 2024 1 1) Nothing
            |> set #code "DUP1"
            |> set #symbolType Stock
            |> set #name "DUP1"
      upsertSymbols [s]
      upsertSymbols [s]
      m <- getSymbolsByTypeMap
      let xs = fromMaybe [] (M.lookup Stock m)
      let codes = map (\(Actual.Symbol { code }) -> code) xs :: [Text]
      length (filter (== "DUP1") codes) `shouldBe` 1
