module Main where

import Prelude
import Test.Hspec

import qualified Tests.Web.Controller.APIControllerSpec
import qualified Tests.Web.Repo.CandleRepoSpec
import qualified Tests.Web.Repo.SymbolRepoSpec
import qualified Tests.Web.Service.CandlesCtxSpec
import qualified Tests.Web.Service.CoveragePolicySpec
import qualified Tests.Web.Service.Process.TTLProcessSpec
import qualified Tests.Web.Service.SymbolsCtxSpec
import qualified Tests.Web.TypesSpec

main :: IO ()
main = hspec do
  Tests.Web.Controller.APIControllerSpec.tests
  Tests.Web.Repo.CandleRepoSpec.tests
  Tests.Web.Repo.SymbolRepoSpec.tests
  Tests.Web.Service.CandlesCtxSpec.tests
  Tests.Web.Service.Process.TTLProcessSpec.tests
  Tests.Web.Service.SymbolsCtxSpec.tests
  Tests.Web.TypesSpec.tests
  Tests.Web.Service.CoveragePolicySpec.tests
