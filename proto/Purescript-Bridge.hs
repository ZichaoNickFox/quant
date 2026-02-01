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
        [ mkSumType (Proxy @SymbolCountsResponse)
        , mkSumType (Proxy @Candle)
        , mkSumType (Proxy @CandlesResponse)
        ]
  writePSTypes outDir bridge types
  postProcessSymbols "purescript/src/Proto/Symbols.purs"

postProcessSymbols :: FilePath -> IO ()
postProcessSymbols path = do
  contents <- readFile path
  let marker = "-- ps-bridge: custom decode wrapper"
      needsBlock = not (marker `isInfixOf` contents)
      contents' = ensureArgonautImport contents
  if needsBlock
    then writeFile path (contents' <> symbolsBlock)
    else pure ()

ensureArgonautImport :: String -> String
ensureArgonautImport contents =
  if "import Data.Argonaut as A" `isInfixOf` contents
    then contents
    else unlines (go (lines contents))
  where
    go (l:ls)
      | "module Proto.Symbols where" `isInfixOf` l =
          l : "import Data.Argonaut as A" : ls
      | otherwise = l : go ls
    go [] = []

symbolsBlock :: String
symbolsBlock = unlines
  [ ""
  , "-- ps-bridge: custom decode wrapper"
  , "newtype SymbolCountsResponseJson = SymbolCountsResponseJson SymbolCountsResponse"
  , ""
  , "instance decodeSymbolsResponse :: DecodeJson SymbolCountsResponseJson where"
  , "  decodeJson v = do"
  , "    obj <- A.decodeJson v"
  , "    complete <- obj A..: \"complete\""
  , "    countsVal <- obj A..: \"counts\""
  , "    pure $ SymbolCountsResponseJson (SymbolCountsResponse { complete, counts: countsVal })"
  ]
