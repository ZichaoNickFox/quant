{-# LANGUAGE OverloadedStrings #-}
module Tests.Integration.Web.Service.ColdWarm.Process.TTLProcessSpec (tests) where

import Config (config)
import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (withContext)
import Prelude
import Test.Hspec
import Web.FrontController ()
import Web.Prelude
import Web.Service.ColdWarm.Process.TTLProcess (needTTL, upsertTTL)
import Web.Service.ColdWarm.SymbolsCtx (SymbolsCtx (..))
import Web.Types (DataFreshness, WebApplication (..))

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "TTLProcess" do
    it "needTTL true when no data_freshness row" $ withContext do
      let ctx = SymbolsCtx { clientId = "client-a" }
      ok <- needTTL ctx
      ok `shouldBe` True

    it "upsertTTL writes row and needTTL becomes false immediately" $ withContext do
      let ctx = SymbolsCtx { clientId = "client-a" }
      upsertTTL ctx
      ok <- needTTL ctx
      ok `shouldBe` False

    it "needTTL false when fresh row exists" $ withContext do
      now <- getCurrentTime
      tz <- getCurrentTimeZone
      let key = Id ("symbols" :: Text) :: Id DataFreshness
      _ <- sqlExec "DELETE FROM data_freshness WHERE dataset_key = ?" (Only key)
      pure ()
      let df = newRecord @DataFreshness
            |> set #datasetKey key
            |> set #lastRefreshedAt (utcToLocalTime tz now)
            |> set #ttlSeconds 604800
      _ <- createRecord df
      let ctx = SymbolsCtx { clientId = "client-a" }
      ok <- needTTL ctx
      ok `shouldBe` False
