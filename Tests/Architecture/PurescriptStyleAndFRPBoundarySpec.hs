module Tests.Architecture.PurescriptStyleAndFRPBoundarySpec (tests) where

import Prelude

import Data.Char (isSpace, toLower)
import Data.List (isInfixOf, isPrefixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

tests :: Spec
tests = do
  describe "Architecture constraints for purescript style and frp" do
    it "all purescript import/export items are case-insensitively sorted" do
      srcFiles <- listPursFiles "purescript/src"
      testFiles <- listPursFiles "purescript/test"
      importOffenders <- fmap concat (mapM findImportOrderOffenders (srcFiles <> testFiles))
      exportOffenders <- fmap concat (mapM findExportOrderOffenders (srcFiles <> testFiles))
      (importOffenders <> exportOffenders) `shouldBe` []

    it "frp files expose createFRP as creation interface" do
      frpFiles <- listPursFiles "purescript/src/FRP"
      offenders <- fmap concat (mapM findNonCreateFRPConstructors frpFiles)
      offenders `shouldBe` []

    it "frp subnetwork files order declarations as Events -> Config -> createFRP" do
      frpFiles <- listPursFiles "purescript/src/FRP"
      offenders <- fmap concat (mapM findFrpStructureOffenders frpFiles)
      offenders `shouldBe` []

findImportOrderOffenders :: FilePath -> IO [String]
findImportOrderOffenders file = do
  content <- readFile file
  let imports =
        [ (lineNo, modName)
        | (lineNo, lineTxt) <- zip [1 :: Int ..] (lines content)
        , Just modName <- [importModuleName lineTxt]
        ]
  pure (adjacentOrderOffenders file "import" imports)

findExportOrderOffenders :: FilePath -> IO [String]
findExportOrderOffenders file = do
  content <- readFile file
  let exports = moduleExportItems content
  pure (adjacentOrderOffenders file "export" (zip [1 :: Int ..] exports))

findNonCreateFRPConstructors :: FilePath -> IO [String]
findNonCreateFRPConstructors file = do
  content <- readFile file
  let offenders =
        [ file <> ":" <> show lineNo <> ": " <> name
        | (lineNo, lineTxt) <- zip [1 :: Int ..] (lines content)
        , Just name <- [topLevelSignatureName lineTxt]
        , "create" `isPrefixOf` name
        , name /= "createFRP"
        ]
  pure offenders

findFrpStructureOffenders :: FilePath -> IO [String]
findFrpStructureOffenders file = do
  content <- readFile file
  let decls = topLevelDeclarations content
      mCreate = firstDeclLine "createFRP" decls
      mEvents = firstDeclLine "Events" decls
      mConfig = firstDeclLine "Config" decls
      firstDecl = case decls of
        [] -> Nothing
        (lineNo, _) : _ -> Just lineNo
  case mCreate of
    Nothing -> pure []
    Just createLine ->
      pure
        ( maybe [] (\eventsLine -> if maybe False (/= eventsLine) firstDecl
            then [file <> ":" <> show eventsLine <> ": Events must be the first top-level declaration in FRP subnetwork files"]
            else []) mEvents
        <> maybe [] (\eventsLine -> maybe [] (\configLine ->
              if eventsLine > configLine
                then [file <> ":" <> show configLine <> ": Config must be declared after Events"]
                else []) mConfig) mEvents
        <> maybe [] (\configLine ->
              if configLine > createLine
                then [file <> ":" <> show createLine <> ": createFRP must be declared after Config"]
                else []) mConfig
        <> maybe [] (\eventsLine ->
              if eventsLine > createLine
                then [file <> ":" <> show createLine <> ": createFRP must be declared after Events"]
                else []) mEvents
        )

adjacentOrderOffenders :: FilePath -> String -> [(Int, String)] -> [String]
adjacentOrderOffenders file label entries =
  [ file <> ":" <> show lineA <> ": " <> label <> " order: " <> a <> " should come after " <> b
  | ((lineA, a), (_, b)) <- zip entries (drop 1 entries)
  , lower a > lower b
  ]

importModuleName :: String -> Maybe String
importModuleName line =
  case words (dropWhile isSpace line) of
    "import" : "qualified" : modName : _ -> Just modName
    "import" : modName : _ -> Just modName
    _ -> Nothing

moduleExportItems :: String -> [String]
moduleExportItems content =
  case moduleHeaderUntilWhere (lines content) of
    Nothing -> []
    Just header ->
      let beforeWhere = takeBefore "where" header
      in case betweenFirstParen beforeWhere of
          Nothing -> []
          Just body -> filter (not . null) (map trim (splitOnComma body))

moduleHeaderUntilWhere :: [String] -> Maybe String
moduleHeaderUntilWhere ls =
  case dropWhile (not . isModuleLine) ls of
    [] -> Nothing
    start ->
      let headerLines = takeUntilWhere start
      in Just (unlines headerLines)
  where
    isModuleLine line = "module " `isPrefixOf` dropWhile isSpace line
    takeUntilWhere [] = []
    takeUntilWhere (x : xs)
      | " where" `isInfixOf` x = [x]
      | otherwise = x : takeUntilWhere xs

betweenFirstParen :: String -> Maybe String
betweenFirstParen txt =
  case break (== '(') txt of
    (_, []) -> Nothing
    (_, _ : rest) ->
      case breakEnd (== ')') rest of
        Nothing -> Nothing
        Just body -> Just body

breakEnd :: (Char -> Bool) -> String -> Maybe String
breakEnd p s =
  let rev = reverse s
  in case break p rev of
      (_, []) -> Nothing
      (insideRev, _ : _) -> Just (reverse insideRev)

takeBefore :: String -> String -> String
takeBefore needle hay =
  go [] hay
  where
    go acc [] = reverse acc
    go acc rest@(c : cs)
      | needle `isPrefixOf` rest = reverse acc
      | otherwise = go (c : acc) cs

splitOnComma :: String -> [String]
splitOnComma s =
  case break (== ',') s of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitOnComma rest

topLevelDeclarations :: String -> [(Int, String)]
topLevelDeclarations content =
  [ (lineNo, name)
  | (lineNo, lineTxt) <- zip [1 :: Int ..] (lines content)
  , Just name <- [topLevelDeclName lineTxt]
  ]

firstDeclLine :: String -> [(Int, String)] -> Maybe Int
firstDeclLine target decls =
  case [ lineNo | (lineNo, name) <- decls, name == target ] of
    [] -> Nothing
    lineNo : _ -> Just lineNo

topLevelDeclName :: String -> Maybe String
topLevelDeclName line =
  case topLevelTypeDeclName line of
    Just name -> Just name
    Nothing -> topLevelSignatureName line

topLevelTypeDeclName :: String -> Maybe String
topLevelTypeDeclName line =
  case words (dropWhile isSpace line) of
    "type" : name : _ -> Just (cleanName name)
    "newtype" : name : _ -> Just (cleanName name)
    "data" : name : _ -> Just (cleanName name)
    "class" : name : _ -> Just (cleanName name)
    _ -> Nothing

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

cleanName :: String -> String
cleanName = takeWhile isNameChar
  where
    isNameChar c = c == '_' || c == '\'' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = reverse . dropWhile p . reverse

lower :: String -> String
lower = map toLower

listPursFiles :: FilePath -> IO [FilePath]
listPursFiles root = do
  allFiles <- listFilesRec root
  pure (filter (\f -> ".purs" `isInfixOf` f) allFiles)

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
