{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate PureScript types for the proto module.
--   Run from repo root:
--     stack runghc proto/Purescript-Bridge.hs
--   or
--     cabal run bridge   (if you add an executable target)
--
--   Output: ps/src/Proto/*.purs
module Main where

import Language.PureScript.Bridge
import Language.PureScript.Bridge.CodeGenSwitches (codeGenSwitches, genSingleModule)

import Proto.Symbols
import Proto.Candles

-- Default mapping covers Text -> String, Map -> Data.Map.Map, Float -> Number, etc.
myBridge :: BridgePart
myBridge = defaultBridge

main :: IO ()
main = do
  let outDir   = "ps/src/Proto"   -- PureScript output directory
      settings = (buildBridge myBridge) { _codeGenSwitches = codeGenSwitches { genSingleModule = False } }
      types    =
        [ mkSumType @SymbolCountsResponse
        , mkSumType @Candle
        , mkSumType @CandlesResponse
        ]
  writePSTypes outDir settings types
