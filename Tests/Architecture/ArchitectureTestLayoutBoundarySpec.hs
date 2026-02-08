module Tests.Architecture.ArchitectureTestLayoutBoundarySpec (tests) where

import Prelude

import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

tests :: Spec
tests = do
  describe "Architecture constraints for test layout" do
    it "constraints document exists" do
      exists <- doesFileExist "Tests/Architecture/CONSTRAINTS.md"
      exists `shouldBe` True

    it "architecture tests are flattened under Tests/Architecture" do
      entries <- listDirectory "Tests/Architecture"
      dirFlags <- mapM (\entry -> (,) entry <$> doesDirectoryExist ("Tests/Architecture" </> entry)) entries
      let subdirs = map fst (filter snd dirFlags)
      subdirs `shouldBe` []
