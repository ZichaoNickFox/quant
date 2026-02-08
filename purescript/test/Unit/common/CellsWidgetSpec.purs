module Test.Unit.Common.CellsWidgetSpec (tests) where

import Common.CellsWidget as W
import Data.Maybe (Maybe(..))
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "buildCellsReadUrl builds expected endpoint query" do
    W.buildCellsReadUrl "strategy" "node-1"
      `shouldEqual` "/CellRead?ownerType=strategy&ownerId=node-1"
    W.buildCellsReadUrl "note" "n-9"
      `shouldEqual` "/CellRead?ownerType=note&ownerId=n-9"

  it "buildCellsReadUrlWithSeq appends cache-busting sequence" do
    W.buildCellsReadUrlWithSeq "strategy" "node-1" 3
      `shouldEqual` "/CellRead?ownerType=strategy&ownerId=node-1&r=3"

  it "withNonce appends _r for urls with and without query string" do
    W.withNonce "/CellUpdate?cellId=c1" 7
      `shouldEqual` "/CellUpdate?cellId=c1&_r=7"
    W.withNonce "/CellMove" 9
      `shouldEqual` "/CellMove?_r=9"

  it "selectedOwnerId is empty for root/unselected and present for node selection" do
    W.selectedOwnerId Nothing
      `shouldEqual` Nothing
    W.selectedOwnerId (Just { ownerId: "node-1", name: "N1" })
      `shouldEqual` Just "node-1"
