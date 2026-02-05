{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import Prelude
import Test.Hspec

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import System.Environment (lookupEnv)
import System.Exit (die)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import IHP.Log.Types (newLogger)
import Web.Prelude (def)
import Web.Fetcher.Python
import Web.Fetcher.FetchSymbols ()
import Web.Fetcher.FetchCandles (CandleRange(..))
import Web.Fetcher.FetchCandles ()
import Generated.Types (Candle)
import Web.Types (Symbol)

assertSymbols :: String -> IO ()
assertSymbols symbolType = do
  logger <- newLogger def
  let ?context = logger
  res <- runPython 10000 "Web/Fetcher/fetch_symbols.py" (Text.pack symbolType) False :: IO (Either PythonError [Symbol])
  case res of
    Right xs | not (null xs) -> pure ()
    Right _ -> die ("symbol_fetcher returned empty list for " <> symbolType)
    Left err -> die ("symbol_fetcher failed for " <> symbolType <> ": " <> show err)

main :: IO ()
main = hspec do
  describe "Smoke: python fetchers" do
    it "symbol_fetcher works via runPython (stock)" do
      assertSymbols "stock"