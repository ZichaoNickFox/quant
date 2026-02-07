module Test.FRP.Component.ListSpec where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "FRP.Component.List" do
    it "placeholder test" do
      1 `shouldEqual` 1
