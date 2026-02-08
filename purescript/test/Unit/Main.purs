module Test.Unit.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude
import Test.Spec (Spec, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Unit.Common.CellsWidgetSpec as CellsWidgetSpec
import Test.Unit.Common.CellWidgetSpec as CellWidgetSpec
import Test.Unit.Common.TreeNodeWidgetSpec as TreeNodeWidgetSpec
import Test.Unit.Common.TreeWidgetSpec as TreeWidgetSpec
import Test.Unit.FRP.CombinatorSpec as CombinatorSpec
import Test.Unit.FRP.Component.InputSpec as InputSpec
import Test.Unit.FRP.RequesterSpec as RequesterSpec
import Test.Unit.Proto.CandlesSpec as CandlesSpec
import Test.Unit.Proto.SseStatusSpec as SseStatusSpec
import Test.Unit.Proto.StrategySpec as StrategySpec
import Test.Unit.Proto.SymbolsSpec as SymbolsSpec
import Test.Unit.Proto.TypesSpec as TypesSpec

spec :: Spec Unit
spec = do
  describe "Common.CellWidget" do
    CellWidgetSpec.tests
  describe "Common.CellsWidget" do
    CellsWidgetSpec.tests
  describe "FRP.Combinator" do
    CombinatorSpec.tests
  describe "Common.TreeWidget" do
    TreeWidgetSpec.tests
  describe "Common.TreeNodeWidget" do
    TreeNodeWidgetSpec.tests
  describe "FRP.Component.Input" do
    InputSpec.tests
  describe "FRP.Requester" do
    RequesterSpec.tests
  describe "Proto.Candles" do
    CandlesSpec.tests
  describe "Proto.SseStatus" do
    SseStatusSpec.tests
  describe "Proto.Strategy" do
    StrategySpec.tests
  describe "Proto.Symbols" do
    SymbolsSpec.tests
  describe "Proto.Types" do
    TypesSpec.tests

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec
