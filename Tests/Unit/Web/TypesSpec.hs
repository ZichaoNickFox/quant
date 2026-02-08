{-# LANGUAGE OverloadedStrings #-}
module Tests.Unit.Web.TypesSpec (tests) where

import Prelude
import Test.Hspec

import Web.Prelude (pathTo)
import Web.Types (NotifyController (..))

tests :: Spec
tests = do
  describe "Web.Types routing" do
    it "NotifyAction path" do
      pathTo NotifyAction `shouldBe` "/sse/notify"
