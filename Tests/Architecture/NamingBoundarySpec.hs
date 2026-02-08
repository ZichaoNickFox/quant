module Tests.Architecture.NamingBoundarySpec (tests) where

import Prelude

import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

tests :: Spec
tests = do
  describe "Architecture constraints for naming boundaries" do
    it "frontend backend calls use request* naming, not fetch*" do
      purescriptFiles <- listFilesRec "purescript/src"
      let businessFiles = filter (not . isInfixOf "/Proto/") purescriptFiles
      offenders <- findTopLevelNameOffenders businessFiles (\name -> "fetch" `isPrefixOf` name)
      offenders `shouldBe` []

    it "backend external fetch modules expose fetch* methods" do
      files <- listDirectory "Web/Fetcher"
      let fetchModules = map ("Web/Fetcher" </>) (filter (\f -> "Fetch" `isPrefixOf` f && ".hs" `isInfixOf` f) files)
      offenders <- findTopLevelNameOffenders fetchModules (\name -> not ("fetch" `isPrefixOf` name))
      offenders `shouldBe` []

findTopLevelNameOffenders :: [FilePath] -> (String -> Bool) -> IO [String]
findTopLevelNameOffenders files isBad = do
  badPerFile <- mapM (findTopLevelNamesInFile isBad) files
  pure (concat badPerFile)

findTopLevelNamesInFile :: (String -> Bool) -> FilePath -> IO [String]
findTopLevelNamesInFile isBad file = do
  content <- readFile file
  let found =
        [ (lineNo, name)
        | (lineNo, lineTxt) <- zip [1 :: Int ..] (lines content)
        , Just name <- [topLevelSignatureName lineTxt]
        , isBad name
        ]
  pure (map (\(lineNo, name) -> file <> ":" <> show lineNo <> ": " <> name) found)

topLevelSignatureName :: String -> Maybe String
topLevelSignatureName line =
  let stripped = dropWhile isSpace line
      (name, rest) = span isNameChar stripped
      restStripped = dropWhile isSpace rest
   in if not (null name) && startsWithLower name && "::" `isPrefixOf` restStripped
        then Just name
        else Nothing
  where
    isNameChar c = c == '_' || c == '\'' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')
    startsWithLower [] = False
    startsWithLower (c : _) = 'a' <= c && c <= 'z'

listFilesRec :: FilePath -> IO [FilePath]
listFilesRec root = do
  entries <- listDirectory root
  fmap concat $ mapM (goFile root) entries
  where
    goFile :: FilePath -> FilePath -> IO [FilePath]
    goFile dir entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then listFilesRec path
        else pure [path]
