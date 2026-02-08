module Tests.Unit.Main (tests) where

import Prelude
import Test.Hspec

import qualified Tests.Unit.Proto.CandlesSpec
import qualified Tests.Unit.Proto.SseStatusSpec
import qualified Tests.Unit.Proto.StrategySpec
import qualified Tests.Unit.Proto.SymbolsSpec
import qualified Tests.Unit.Web.Service.ColdWarm.CandlesCtxSpec
import qualified Tests.Unit.Web.Service.ColdWarm.CoveragePolicySpec
import qualified Tests.Unit.Web.Service.ColdWarm.Infrastructure.NotifyHubSpec
import qualified Tests.Unit.Web.Service.ColdWarm.SymbolsCtxSpec
import qualified Tests.Unit.Web.TypesSpec

tests :: Spec
tests = do
  Tests.Unit.Proto.CandlesSpec.tests
  Tests.Unit.Proto.SseStatusSpec.tests
  Tests.Unit.Proto.StrategySpec.tests
  Tests.Unit.Proto.SymbolsSpec.tests
  Tests.Unit.Web.Service.ColdWarm.CandlesCtxSpec.tests
  Tests.Unit.Web.Service.ColdWarm.CoveragePolicySpec.tests
  Tests.Unit.Web.Service.ColdWarm.Infrastructure.NotifyHubSpec.tests
  Tests.Unit.Web.Service.ColdWarm.SymbolsCtxSpec.tests
  Tests.Unit.Web.TypesSpec.tests
