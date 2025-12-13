{-# LANGUAGE TemplateHaskell #-}

module Service.Provider.SymbolProvider
  ( provideSymbols
  , provideSymbolsSQL
  ) where

import           Data.Aeson
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text hiding (length)
import qualified Data.Vector as V
import           Data.UUID (UUID)
import           Database.PostgreSQL.Simple.ToField (ToField(..), toField)
import           Database.PostgreSQL.Simple.ToRow (ToRow(..))
import           Database.PostgreSQL.Simple.Types (Query (..))
import           Generated.Types
import           GHC.Generics
import           IHP.Controller.Context
import           IHP.HaskellSupport
import           IHP.Log
import           IHP.Log.Types
import           IHP.ModelSupport
import           Prelude
import           Service.Provider.Python

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
    listDate <- o.: "list_date"
    delistDate <- o .: "delist_date"
    pure $ newRecord @Symbol
      |> set #code code
      |> set #name name
      |> set #symbolType symbolType
      |> set #listDate listDate
      |> set #delistDate delistDate

instance ToRow Symbol where
  toRow s =
    [ toField (get #code s)
    , toField (get #name s)
    , toField (get #symbolType s)
    , toField (get #listDate s)
    , toField (get #delistDate s)
    ]

provideSymbols :: (?context :: ControllerContext, LoggingProvider ControllerContext) => SymbolType -> IO [Symbol]
provideSymbols symbolType = do
  result <- fromJust <$> (runPython "Service/Provider/symbol_provider.py" symbolType False :: IO (Maybe [Symbol]))
  info $ "[provideSymbols] nums - " <> show (length result)
  return result

provideSymbolsSQL :: IO Query
provideSymbolsSQL = do
  sql <- readFile "Service/Provider/SymbolProvider.sql"
  return $ Query (cs sql)