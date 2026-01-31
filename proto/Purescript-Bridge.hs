{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate PureScript types for the proto module.
--   Run from repo root:
--      runghc proto/Purescript-Bridge.hs
--   Output:
--      ps/src/Proto/*.purs
module Main where

import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge

import Proto.Symbols
import Proto.Candles

-- Default mapping covers Text -> String, Map -> Data.Map.Map, Float -> Number, etc.
myBridge :: BridgePart
myBridge = defaultBridge

main :: IO ()
main = do
  let outDir = "ps/src/Proto"   -- PureScript output directory
      bridge = buildBridge myBridge
      types  =
        [ mkSumType (Proxy @SymbolCountsResponse)
        , mkSumType (Proxy @Candle)
        , mkSumType (Proxy @CandlesResponse)
        ]
  writePSTypes outDir bridge types
