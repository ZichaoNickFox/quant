{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate PureScript types for the proto module.
--   Run from repo root:
--      runghc proto/Purescript-Bridge.hs
--   Output:
--      purescript/src/Proto/*.purs
module Main where

import Data.List (isInfixOf)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Language.PureScript.Bridge

import Proto.Symbols
import Proto.Candles
import Proto.Strategy

-- Default mapping covers Text -> String, Map -> Data.Map.Map, Float -> Number, etc.
myBridge :: BridgePart
myBridge = defaultBridge

main :: IO ()
main = do
  let outDir = "purescript/src"   -- PureScript output directory
      bridge = buildBridge myBridge
      types  =
        [ mkSumType (Proxy @SymbolInfo)
        , mkSumType (Proxy @APISymbolsResponse)
        , mkSumType (Proxy @Candle)
        , mkSumType (Proxy @CandlesResponse)
        , mkSumType (Proxy @StrategyInfo)
        ]
  writePSTypes outDir bridge types
  postProcessAllProto "purescript/src/Proto"

postProcessAllProto :: FilePath -> IO ()
postProcessAllProto dir = do
  files <- listDirectory dir
  let pursFiles = [ dir </> f | f <- files, takeExtension f == ".purs" ]
  mapM_ ensureShowInstancesFile pursFiles

ensureShowInstancesFile :: FilePath -> IO ()
ensureShowInstancesFile path = do
  contents <- readFile path
  let contents' =
        sortImportBlock
          (ensureDecodeHelpers
            (ensureEitherImport (ensureJsonImport (ensureGenericShowImport (ensureShowInstances contents)))))
  if contents' == contents then pure () else writeFile path contents'

ensureShowInstances :: String -> String
ensureShowInstances contents = unlines (go (lines contents))
  where
    go (l:ls)
      | "derive instance show" `isInfixOf` l =
          case extractShowType l of
            Nothing -> l : go ls
            Just typeName ->
              let showLine = "instance show" <> typeName <> " :: Show " <> typeName <> " where"
                  showBody = "  show = genericShow"
              in showLine : showBody : go ls
      | "derive instance generic" `isInfixOf` l =
          case extractGenericType l of
            Nothing ->
              l : go ls
            Just typeName ->
              let showLine = "instance show" <> typeName <> " :: Show " <> typeName <> " where"
                  hasShow = showLine `isInfixOf` contents
                  rest = go ls
              in if hasShow then l : rest else l : showLine : "  show = genericShow" : rest
      | otherwise = l : go ls
    go [] = []

    extractGenericType :: String -> Maybe String
    extractGenericType line =
      let ws = words line
          idx = fromMaybe (-1) (elemIndex "Generic" ws)
      in if idx >= 0 && idx + 1 < length ws then Just (ws !! (idx + 1)) else Nothing

    extractShowType :: String -> Maybe String
    extractShowType line =
      let ws = words line
          idx = fromMaybe (-1) (elemIndex "Show" ws)
      in if idx >= 0 && idx + 1 < length ws then Just (ws !! (idx + 1)) else Nothing

    elemIndex :: Eq a => a -> [a] -> Maybe Int
    elemIndex x = goIdx 0
      where
        goIdx _ [] = Nothing
        goIdx i (y:ys)
          | x == y = Just i
          | otherwise = goIdx (i + 1) ys

ensureGenericShowImport :: String -> String
ensureGenericShowImport contents =
  if "import Data.Show.Generic (genericShow)" `isInfixOf` contents
    then contents
    else unlines (go (lines contents))
  where
    go (l:ls)
      | "import Data.Generic.Rep" `isInfixOf` l =
          l : "import Data.Show.Generic (genericShow)" : ls
      | otherwise = l : go ls
    go [] = []

ensureJsonImport :: String -> String
ensureJsonImport contents =
  if "import Data.Argonaut.Core (Json)" `isInfixOf` contents
    then contents
    else unlines (go (lines contents))
  where
    go (l:ls)
      | "import Data.Argonaut.Decode.Class" `isInfixOf` l =
          l : "import Data.Argonaut.Core (Json)" : ls
      | otherwise = l : go ls
    go [] = []

ensureEitherImport :: String -> String
ensureEitherImport contents =
  if "import Data.Either (Either(..))" `isInfixOf` contents
    then contents
    else unlines (go (lines contents))
  where
    go (l:ls)
      | "import Data.Argonaut.Core (Json)" `isInfixOf` l =
          l : "import Data.Either (Either(..))" : ls
      | otherwise = l : go ls
    go [] = []

ensureDecodeHelpers :: String -> String
ensureDecodeHelpers contents =
  let typeNames = extractNewtypeNames (lines contents)
  in foldl addHelper contents typeNames
  where
    addHelper acc typeName =
      let helperName = "decodeMaybe" <> typeName
      in if helperName `isInfixOf` acc
          then acc
          else acc <> "\n" <> decodeHelperBlock typeName

    decodeHelperBlock typeName =
      unlines
        [ helperName <> " :: Json -> Maybe " <> typeName
        , helperName <> " json ="
        , "  case decodeJson json of"
        , "    Right v -> Just v"
        , "    Left _ -> Nothing"
        ]
      where
        helperName = "decodeMaybe" <> typeName

    extractNewtypeNames :: [String] -> [String]
    extractNewtypeNames = foldr pick []
      where
        pick line acc =
          case words line of
            "newtype" : typeName : _ -> typeName : acc
            _ -> acc

sortImportBlock :: String -> String
sortImportBlock contents = unlines (go (lines contents))
  where
    go :: [String] -> [String]
    go ls =
      let (prefix, rest) = break isImportLine ls
      in case rest of
          [] -> ls
          _ ->
            let (importRegion, suffix) = span isImportOrBlank rest
                imports = filter isImportLine importRegion
                sortedImports = sortByCI imports
                spacer = case suffix of
                  [] -> []
                  (next : _) -> if null next then [] else [""]
            in prefix <> sortedImports <> spacer <> suffix

    isImportLine line = take 7 (dropWhile (== ' ') line) == "import "
    isImportOrBlank line = isImportLine line || null line

    sortByCI [] = []
    sortByCI (x : xs) = insertCI x (sortByCI xs)

    insertCI x [] = [x]
    insertCI x (y : ys)
      | cmpCI x y <= EQ = x : y : ys
      | otherwise = y : insertCI x ys

    cmpCI a b = compare (map toLower a) (map toLower b)
