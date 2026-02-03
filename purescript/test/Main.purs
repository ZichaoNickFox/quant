module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.FRP.CombinatorSpec as CombinatorSpec
import Test.FRP.RequesterSpec as RequesterSpec
import Test.Spec (Spec, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

spec :: Spec Unit
spec = do
  describe "FRP.Combinator" do
    CombinatorSpec.tests
  describe "FRP.Requester" do
    RequesterSpec.tests

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec
