{-# LANGUAGE OverloadedStrings #-}
module Tests.Web.Controller.APIControllerSpec (tests) where

import Config (config)
import qualified Data.Aeson as A
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import qualified Data.Vector as V
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (callActionWithParams, responseBody, withContext)
import Prelude
import Test.Hspec
import Web.Controller.APIController ()
import Web.FrontController ()
import Web.Prelude
import Web.Types

mkTime :: Integer -> Int -> Int -> LocalTime
mkTime y m d = LocalTime (fromGregorian y m d) (TimeOfDay 0 0 0)

insertSymbolsTTL :: (?modelContext :: ModelContext) => IO ()
insertSymbolsTTL = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let key = Id ("symbols" :: Text) :: Id DataFreshness
      df = newRecord @DataFreshness
        |> set #datasetKey key
        |> set #lastRefreshedAt (utcToLocalTime tz now)
        |> set #ttlSeconds 604800
  _ <- createRecord df
  pure ()

insertSymbolWithCandles :: (?modelContext :: ModelContext) => IO ()
insertSymbolWithCandles = do
  sym <-
    newRecord @Symbol
      |> set #code "AAPL"
      |> set #symbolType Stock
      |> set #name "Apple"
      |> set #listDate (mkTime 2024 1 1)
      |> set #delistDate Nothing
      |> createRecord
  let (Id sid) = get #id sym
      c1 =
        newRecord @Candle
          |> set #symbolId sid
          |> set #timeframe 1
          |> set #datetime (mkTime 2024 1 1)
          |> set #open 1
          |> set #close 1
          |> set #high 1
          |> set #low 1
          |> set #volume 1
          |> set #amount 1
      c2 =
        newRecord @Candle
          |> set #symbolId sid
          |> set #timeframe 1
          |> set #datetime (mkTime 2024 1 2)
          |> set #open 2
          |> set #close 2
          |> set #high 2
          |> set #low 2
          |> set #volume 2
          |> set #amount 2
  _ <- createRecord c1
  _ <- createRecord c2
  pure ()

expectObjectHasKeys :: [Key] -> LBS.ByteString -> Expectation
expectObjectHasKeys keys body =
  case A.decode body of
    Just (A.Object obj) ->
      mapM_ (\k -> KM.member k obj `shouldBe` True) keys
    _ -> expectationFailure ("expected JSON object, got: " <> cs body)

expectArrayLength :: Key -> Int -> LBS.ByteString -> Expectation
expectArrayLength key expected body =
  case A.decode body of
    Just (A.Object obj) ->
      case KM.lookup key obj of
        Just (A.Array arr) -> V.length arr `shouldBe` expected
        _ -> expectationFailure ("expected array at key, got: " <> cs body)
    _ -> expectationFailure ("expected JSON object, got: " <> cs body)

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "APIController (integration)" do
    it "APISymbols returns symbols field" $ withContext do
      insertSymbolsTTL
      response <- callActionWithParams APISymbolsAction []
      body <- responseBody response
      expectObjectHasKeys ["symbols"] body

    it "APICandles returns expected fields" $ withContext do
      insertSymbolWithCandles
      let params =
            [ ("symbolCode", "AAPL")
            , ("symbolType", "stock")
            , ("timeframe", "1")
            , ("from", "2024-01-01")
            , ("to", "2024-01-02")
            ]
      response <- callActionWithParams APICandlesAction params
      body <- responseBody response
      expectObjectHasKeys ["symbolType", "symbolCode", "timeframe", "data"] body
      expectArrayLength "data" 2 body
