{-# LANGUAGE OverloadedStrings #-}
module Tests.Web.Repo.PageRepoSpec (tests) where

import Config (config)
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (withContext)
import Prelude
import Test.Hspec
import Web.FrontController ()
import Web.Prelude
import Web.Repo.PageRepo as PageRepo
import Web.Types

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "PageRepo (integration)" do
    it "getOrCreateNote creates once and then returns existing row" $ withContext do
      n1 <- PageRepo.getOrCreateNote
      n2 <- PageRepo.getOrCreateNote
      get #id n1 `shouldBe` get #id n2
      count <- query @Note |> fetchCount
      count `shouldBe` 1

    it "getOrCreateStrategy creates once and then returns existing row" $ withContext do
      s1 <- PageRepo.getOrCreateStrategy
      s2 <- PageRepo.getOrCreateStrategy
      get #id s1 `shouldBe` get #id s2
      count <- query @Strategy |> fetchCount
      count `shouldBe` 1
