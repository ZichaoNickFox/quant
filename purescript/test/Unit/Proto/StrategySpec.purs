module Test.Unit.Proto.StrategySpec (tests) where

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Prelude
import Proto.Strategy (StrategyInfo(..))
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "roundtrips StrategyInfo via JSON" do
    let s = StrategyInfo { id: "sid-1", name: "策略-1" }
    case decodeJson (encodeJson s) of
      Right (StrategyInfo r) -> do
        r.id `shouldEqual` "sid-1"
        r.name `shouldEqual` "策略-1"
      Left _ -> shouldEqual true false
