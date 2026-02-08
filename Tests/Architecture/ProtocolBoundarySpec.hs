module Tests.Architecture.ProtocolBoundarySpec (tests) where

import Prelude

import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

tests :: Spec
tests = do
  describe "Architecture constraints for protocol boundaries" do
    it "protocol definition and ser/de code lives only in proto, bridge, or fetcher modules" do
      allHsFiles <- listFilesRec "Web"
      allPursFiles <- listFilesRec "purescript/src"
      let hsFiles = filter (\f -> ".hs" `isInfixOf` f) allHsFiles
          nonFetcherHsFiles = filter (not . isInfixOf "Web/Fetcher/") hsFiles
          pursFiles = filter (\f -> ".purs" `isInfixOf` f) allPursFiles
          nonProtoPurs = filter (not . isInfixOf "/Proto/") pursFiles
      hsOffenders <- findPatternOffenders hsProtocolMarkers nonFetcherHsFiles
      pursOffenders <- findPatternOffenders pursProtocolMarkers nonProtoPurs
      (hsOffenders <> pursOffenders) `shouldBe` []

    it "haskell proto modules do not import business modules" do
      protoHs <- listFilesRec "proto"
      offenders <- findForbiddenImports protoHs hsForbiddenProtoImports
      offenders `shouldBe` []

    it "purescript proto modules do not import business modules" do
      protoPurs <- listFilesRec "purescript/src/Proto"
      offenders <- findForbiddenImports protoPurs pursForbiddenProtoImports
      offenders `shouldBe` []

findPatternOffenders :: [String] -> [FilePath] -> IO [String]
findPatternOffenders patterns files = do
  badPerFile <- mapM (findPatternInFile patterns) files
  pure (concat badPerFile)

findPatternInFile :: [String] -> FilePath -> IO [String]
findPatternInFile patterns file = do
  content <- readFile file
  let offenders =
        [ file <> ":" <> show lineNo <> ": " <> marker
        | (lineNo, lineTxt) <- zip [1 :: Int ..] (lines content)
        , marker <- patterns
        , marker `isInfixOf` lineTxt
        ]
  pure offenders

findForbiddenImports :: [FilePath] -> [String] -> IO [String]
findForbiddenImports files forbidden = do
  badPerFile <- mapM (findForbiddenImportsInFile forbidden) files
  pure (concat badPerFile)

findForbiddenImportsInFile :: [String] -> FilePath -> IO [String]
findForbiddenImportsInFile forbidden file = do
  content <- readFile file
  let offenders =
        [ file <> ":" <> show lineNo <> ": import " <> modName
        | (lineNo, lineTxt) <- zip [1 :: Int ..] (lines content)
        , Just modName <- [importModuleName lineTxt]
        , any (`isPrefixOf` modName) forbidden
        ]
  pure offenders

importModuleName :: String -> Maybe String
importModuleName line =
  case words (dropWhile isSpace line) of
    "import" : "qualified" : modName : _ -> Just modName
    "import" : modName : _ -> Just modName
    _ -> Nothing

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

hsProtocolMarkers :: [String]
hsProtocolMarkers =
  [ "instance ToJSON"
  , "instance FromJSON"
  , "genericToJSON"
  , "genericParseJSON"
  , "eitherDecode"
  ]

pursProtocolMarkers :: [String]
pursProtocolMarkers =
  [ "instance encodeJson"
  , "instance decodeJson"
  , "genericEncodeAeson"
  , "genericDecodeAeson"
  ]

hsForbiddenProtoImports :: [String]
hsForbiddenProtoImports =
  [ "Web."
  , "Application."
  ]

pursForbiddenProtoImports :: [String]
pursForbiddenProtoImports =
  [ "Common."
  , "DataPage"
  , "StrategyPage"
  , "FRP."
  , "FFI."
  , "Web."
  ]
