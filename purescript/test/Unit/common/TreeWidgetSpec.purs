module Test.Unit.Common.TreeWidgetSpec (tests) where

import Common.TreeWidget as W
import Data.Maybe (Maybe(..))
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "buildCreateUrl builds root and child create endpoints" do
    W.buildCreateUrl "strategy" "owner-1" ""
      `shouldEqual` "/StrategyTreeCreate?strategyId=owner-1"
    W.buildCreateUrl "strategy" "owner-1" "parent-1"
      `shouldEqual` "/StrategyTreeCreate?strategyId=owner-1&parentTreeId=parent-1"

  it "buildUpdateUrl includes parent only when present" do
    W.buildUpdateUrl { externalId: "n1", parentExternalId: Just "p1", nodeOrder: 2 }
      `shouldEqual` "/StrategyTreeUpdate?treeId=n1&nodeOrder=2&parentTreeId=p1"
    W.buildUpdateUrl { externalId: "n1", parentExternalId: Nothing, nodeOrder: 2 }
      `shouldEqual` "/StrategyTreeUpdate?treeId=n1&nodeOrder=2"
