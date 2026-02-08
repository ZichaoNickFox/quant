module Tests.Architecture.PurescriptTestDirectoryBoundarySpec (tests) where

import Prelude

import Data.List ((\\), nub, sort)
import Data.Maybe (mapMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), makeRelative, normalise)
import Test.Hspec

tests :: Spec
tests = do
  describe "Architecture constraints for purescript test directories" do
    it "unit test directory structure mirrors src" do
      assertMirrorsSrc "purescript/test/Unit"

    it "integration test directory structure mirrors src" do
      assertMirrorsSrc "purescript/test/Integration"

assertMirrorsSrc :: FilePath -> Expectation
assertMirrorsSrc testRoot = do
  srcDirs <- relativeDirs "purescript/src"
  testDirs <- relativeDirs testRoot
  let missing = sort (srcDirs \\ testDirs)
      extra = sort (testDirs \\ srcDirs)
      err =
        unlines
          [ "source dirs: " <> show srcDirs
          , "test dirs: " <> show testDirs
          , "missing in " <> testRoot <> ": " <> show missing
          , "extra in " <> testRoot <> ": " <> show extra
          ]
  if null missing && null extra
    then pure ()
    else expectationFailure err

relativeDirs :: FilePath -> IO [FilePath]
relativeDirs root = do
  dirs <- listDirsRec root
  let toRel dir =
        let rel = normalise (makeRelative root dir)
         in if rel == "." then Nothing else Just rel
  pure (sort (nub (mapMaybe toRel dirs)))

listDirsRec :: FilePath -> IO [FilePath]
listDirsRec root = do
  entries <- listDirectory root
  childDirs <- mapM (goDir root) entries
  pure (root : concat childDirs)
  where
    goDir :: FilePath -> FilePath -> IO [FilePath]
    goDir dir entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then listDirsRec path
        else pure []
