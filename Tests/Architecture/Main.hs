module Tests.Architecture.Main (main, tests) where

import Prelude
import Test.Hspec

import qualified Tests.Architecture.ArchitectureTestLayoutBoundarySpec
import qualified Tests.Architecture.ControllerBoundarySpec
import qualified Tests.Architecture.NamingBoundarySpec
import qualified Tests.Architecture.ProtocolBoundarySpec
import qualified Tests.Architecture.PurescriptStyleAndFRPBoundarySpec
import qualified Tests.Architecture.PurescriptTestDirectoryBoundarySpec

main :: IO ()
main = hspec tests

tests :: Spec
tests = do
  Tests.Architecture.ArchitectureTestLayoutBoundarySpec.tests
  Tests.Architecture.ControllerBoundarySpec.tests
  Tests.Architecture.NamingBoundarySpec.tests
  Tests.Architecture.ProtocolBoundarySpec.tests
  Tests.Architecture.PurescriptStyleAndFRPBoundarySpec.tests
  Tests.Architecture.PurescriptTestDirectoryBoundarySpec.tests
