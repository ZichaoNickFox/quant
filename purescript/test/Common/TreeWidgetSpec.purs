module Test.Common.TreeWidgetSpec (tests) where

import Common.TreeWidget as W
import Data.Maybe (Maybe(..))
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "buildUpdateUrl includes parent only when present" do
    W.buildUpdateUrl { treeId: "n1", parentTreeId: Just "p1", nodeOrder: 2 }
      `shouldEqual` "/TreeUpdate?treeId=n1&nodeOrder=2&parentTreeId=p1"
    W.buildUpdateUrl { treeId: "n1", parentTreeId: Nothing, nodeOrder: 2 }
      `shouldEqual` "/TreeUpdate?treeId=n1&nodeOrder=2"

  it "buildRenameUrl appends encoded name while preserving parent/order fields" do
    W.buildRenameUrl "n1" "new name" (Just "p1") 2
      `shouldEqual` "/TreeUpdate?treeId=n1&nodeOrder=2&parentTreeId=p1&name=new%20name"
    W.buildRenameUrl "n1" "A" Nothing 3
      `shouldEqual` "/TreeUpdate?treeId=n1&nodeOrder=3&name=A"
