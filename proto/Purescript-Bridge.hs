{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate PureScript types for the proto module.
--   Run from repo root:
--      runghc proto/Purescript-Bridge.hs
--   Output:
--      purescript/src/Proto/*.purs
module Main where

import Data.Proxy (Proxy (..))
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Language.PureScript.Bridge

import Proto.Symbols
import Proto.Candles

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
  let contents' = ensureGenericShowImport (ensureShowInstances contents)
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
