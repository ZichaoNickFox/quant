module Data.API
  ( getSymbols
  ) where

import           Data.Aeson
import           Data.Python
import           Data.Maybe
import           Data.Text hiding (length)
import qualified Data.Vector as V
import           Data.UUID (UUID)
import           Generated.Types
import           GHC.Generics
import           IHP.Controller.Context
import           IHP.HaskellSupport
import           IHP.Log
import           IHP.Log.Types
import           IHP.ModelSupport
import           Prelude

-- getCandle :: CandleRange -> IO (V.Vector Candle)
-- getCandle candleRange = do
--   result <- V.fromList . fromJust <$> runPython "./Script/get_candle.py" candleRange False
--   return result
instance ToJSON SymbolType where
  toJSON = \case
    Stock  -> "stock"
    Etf    -> "etf"
    Future -> "future"
    Option -> "option"
    Fund   -> "fund"

instance FromJSON SymbolType where
  parseJSON = withText "SymbolType" $ \case
    "stock"  -> pure Stock
    "etf"    -> pure Etf
    "future" -> pure Future
    "option" -> pure Option
    "fund"   -> pure Fund
    other    -> fail $ "Unknown SymbolType: " <> show other

instance FromJSON Symbol where
  parseJSON = withObject "Symbol" $ \o -> do
    code <- o .: "code"
    name <- o .: "name"
    symbolType <- o .: "symbol_type"
    pure $ newRecord @Symbol
      |> set #code code
      |> set #name name
      |> set #symbolType symbolType
getSymbols :: (?context :: ControllerContext, LoggingProvider ControllerContext) => SymbolType -> IO [Symbol]
getSymbols symbolType = do
  result <- fromJust <$> (runPython "./Data/Script/get_symbols.py" symbolType False :: IO (Maybe [Symbol]))
  info $ "[getSymbols] nums - " <> show (length result)
  return result