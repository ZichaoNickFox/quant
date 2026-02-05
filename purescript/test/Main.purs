module Test.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude
import Test.Common.CellWidgetSpec as CellWidgetSpec
import Test.Common.CellsWidgetSpec as CellsWidgetSpec
import Test.Common.TreeWidgetSpec as TreeWidgetSpec
import Test.FRP.CombinatorSpec as CombinatorSpec
import Test.FRP.InputTextSpec as InputTextSpec
import Test.FRP.RequesterSpec as RequesterSpec
import Test.FRP.TreeSpec as TreeSpec
import Test.Proto.CandlesSpec as CandlesSpec
import Test.Proto.SseStatusSpec as SseStatusSpec
import Test.Proto.SymbolsSpec as SymbolsSpec
import Test.Proto.TypesSpec as TypesSpec
import Test.Spec (Spec, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

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
  describe "FRP.InputText" do
    InputTextSpec.tests
  describe "FRP.Requester" do
    RequesterSpec.tests
  describe "FRP.Tree" do
    TreeSpec.tests
  describe "Proto.Candles" do
    CandlesSpec.tests
  describe "Proto.SseStatus" do
    SseStatusSpec.tests
  describe "Proto.Symbols" do
    SymbolsSpec.tests
  describe "Proto.Types" do
    TypesSpec.tests

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec
