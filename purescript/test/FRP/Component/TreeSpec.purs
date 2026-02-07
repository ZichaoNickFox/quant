module Test.FRP.Component.TreeSpec where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "FRP.Component.Tree" do
    it "placeholder test" do
      1 `shouldEqual` 1
