module Test.Common.TreeWidgetSpec (tests) where

import Common.TreeWidget as W
import Data.Maybe (Maybe(..))
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "buildCreateUrl builds root and child create endpoints" do
    W.buildCreateUrl "strategy" "owner-1" ""
      `shouldEqual` "/TreeCreate?ownerType=strategy&ownerId=owner-1&nodeType=file&name=NewNode"
    W.buildCreateUrl "strategy" "owner-1" "parent-1"
      `shouldEqual` "/TreeCreate?ownerType=strategy&ownerId=owner-1&nodeType=file&name=NewNode&parentTreeId=parent-1"

  it "buildUpdateUrl includes parent only when present" do
    W.buildUpdateUrl { externalId: "n1", parentExternalId: Just "p1", nodeOrder: 2 }
      `shouldEqual` "/TreeUpdate?treeId=n1&nodeOrder=2&parentTreeId=p1"
    W.buildUpdateUrl { externalId: "n1", parentExternalId: Nothing, nodeOrder: 2 }
      `shouldEqual` "/TreeUpdate?treeId=n1&nodeOrder=2"
