module Test.Common.CellWidgetSpec (tests) where

import Common.CellWidget as W
import Data.Argonaut.Core as J
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "decodeCellsFromJson supports snake_case payloads" do
    let json = J.fromArray
          [ J.fromObject
              ( FO.fromFoldable
                  [ Tuple "cell_type" (J.fromString "Raw")
                  , Tuple "id" (J.fromString "c1")
                  , Tuple "cell_order" (J.fromNumber 1.0)
                  , Tuple "content" (J.fromString "hello")
                  ]
              )
          ]
    W.decodeCellsFromJson json `shouldEqual`
      [ { id: "c1", cellType: "Raw", cellOrder: 1, content: Just "hello" } ]

  it "decodeCellsFromJson supports camelCase payloads and null content" do
    let json = J.fromArray
          [ J.fromObject
              ( FO.fromFoldable
                  [ Tuple "cellType" (J.fromString "Image")
                  , Tuple "id" (J.fromString "c2")
                  , Tuple "cellOrder" (J.fromNumber 2.0)
                  , Tuple "content" J.jsonNull
                  ]
              )
          ]
    W.decodeCellsFromJson json `shouldEqual`
      [ { id: "c2", cellType: "Image", cellOrder: 2, content: Nothing } ]

  it "decodeCellsFromJson drops invalid cells and returns [] for non-array" do
    let mixed = J.fromArray
          [ J.fromObject
              ( FO.fromFoldable
                  [ Tuple "cellType" (J.fromString "Backtest")
                  , Tuple "id" (J.fromString "c3")
                  , Tuple "cellOrder" (J.fromNumber 3.0)
                  ]
              )
          , J.fromObject
              ( FO.fromFoldable
                  [ Tuple "cellType" (J.fromString "Bad")
                  , Tuple "id" (J.fromString "c4")
                  , Tuple "cellOrder" (J.fromString "x")
                  ]
              )
          ]
    W.decodeCellsFromJson mixed `shouldEqual`
      [ { id: "c3", cellType: "Backtest", cellOrder: 3, content: Nothing } ]
    W.decodeCellsFromJson (J.fromString "oops") `shouldEqual` []

  it "buildCellUpdateInput returns Nothing when cellId is empty" do
    W.buildCellUpdateInput "" "raw" "abc" `shouldEqual` Nothing

  it "buildCellUpdateInput returns Just when cellId is present" do
    W.buildCellUpdateInput "c1" "chart" "[1,2,3]" `shouldEqual`
      Just { cellId: "c1", cellType: "chart", content: "[1,2,3]" }

  it "cellTypeOptions includes chart type and label" do
    W.cellTypeOptions `shouldEqual`
      [ Tuple "raw" "Raw"
      , Tuple "image" "Image"
      , Tuple "backtest" "Backtest"
      , Tuple "chart" "Chart"
      ]

  it "isChartCellType supports chart and legacy lightweight_charts" do
    W.isChartCellType "chart" `shouldEqual` true
    W.isChartCellType "lightweight_charts" `shouldEqual` true
    W.isChartCellType "raw" `shouldEqual` false
