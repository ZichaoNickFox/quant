module Tests.Integration.Main (tests) where

import Prelude
import Test.Hspec

import qualified Tests.Integration.Web.Controller.ColdWarmControllerSpec
import qualified Tests.Integration.Web.Controller.CellControllerSpec
import qualified Tests.Integration.Web.Controller.NotifyControllerSpec
import qualified Tests.Integration.Web.Controller.StrategyControllerSpec
import qualified Tests.Integration.Web.Fetcher.PythonSpec
import qualified Tests.Integration.Web.Repo.CandleRepoSpec
import qualified Tests.Integration.Web.Repo.CellRepoSpec
import qualified Tests.Integration.Web.Repo.SymbolRepoSpec
import qualified Tests.Integration.Web.Repo.TreeRepoSpec
import qualified Tests.Integration.Web.Service.ColdWarm.Process.ServiceProcessSpec
import qualified Tests.Integration.Web.Service.ColdWarm.Process.TTLProcessSpec

tests :: Spec
tests = do
  Tests.Integration.Web.Controller.ColdWarmControllerSpec.tests
  Tests.Integration.Web.Controller.CellControllerSpec.tests
  Tests.Integration.Web.Controller.NotifyControllerSpec.tests
  Tests.Integration.Web.Controller.StrategyControllerSpec.tests
  Tests.Integration.Web.Fetcher.PythonSpec.tests
  Tests.Integration.Web.Repo.CandleRepoSpec.tests
  Tests.Integration.Web.Repo.CellRepoSpec.tests
  Tests.Integration.Web.Repo.SymbolRepoSpec.tests
  Tests.Integration.Web.Repo.TreeRepoSpec.tests
  Tests.Integration.Web.Service.ColdWarm.Process.ServiceProcessSpec.tests
  Tests.Integration.Web.Service.ColdWarm.Process.TTLProcessSpec.tests
