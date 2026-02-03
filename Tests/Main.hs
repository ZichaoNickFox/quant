module Main where

import Prelude
import Test.Hspec

import qualified Tests.Proto.CandlesSpec
import qualified Tests.Proto.SseStatusSpec
import qualified Tests.Proto.SymbolsSpec
import qualified Tests.Web.Controller.APIControllerSpec
import qualified Tests.Web.Controller.NotifyControllerSpec
import qualified Tests.Web.Repo.CandleRepoSpec
import qualified Tests.Web.Repo.SymbolRepoSpec
import qualified Tests.Web.Service.CandlesCtxSpec
import qualified Tests.Web.Service.CoveragePolicySpec
import qualified Tests.Web.Service.Infrastructure.NotifyHubSpec
import qualified Tests.Web.Service.Process.TTLProcessSpec
import qualified Tests.Web.Service.SymbolsCtxSpec
import qualified Tests.Web.TypesSpec

main :: IO ()
main = hspec do
  Tests.Proto.CandlesSpec.tests
  Tests.Proto.SseStatusSpec.tests
  Tests.Proto.SymbolsSpec.tests
  Tests.Web.Controller.APIControllerSpec.tests
  Tests.Web.Controller.NotifyControllerSpec.tests
  Tests.Web.Repo.CandleRepoSpec.tests
  Tests.Web.Repo.SymbolRepoSpec.tests
  Tests.Web.Service.CandlesCtxSpec.tests
  Tests.Web.Service.CoveragePolicySpec.tests
  Tests.Web.Service.Infrastructure.NotifyHubSpec.tests
  Tests.Web.Service.Process.TTLProcessSpec.tests
  Tests.Web.Service.SymbolsCtxSpec.tests
  Tests.Web.TypesSpec.tests
