module Test.Integration.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

spec :: Spec Unit
spec = pure unit

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec
