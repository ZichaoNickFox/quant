{-# LANGUAGE TemplateHaskell #-}

module Web.Fetcher.SymbolFetcher
  ( fetchSymbols
  ) where

import           Data.Aeson
import           Data.String.Conversions
import           Data.Text hiding (length)
import qualified Data.Text                      as Text
import           Database.PostgreSQL.Simple.ToField (ToField(..), toField)
import           Database.PostgreSQL.Simple.ToRow (ToRow(..))
import           Database.PostgreSQL.Simple.Types (Query (..))
import           Web.Fetcher.Python
import           Web.Prelude
import           Web.Types

instance ToJSON SymbolType where
  toJSON = \case
    Stock  -> "stock"
    Index  -> "index"
    Etf    -> "etf"
    Future -> "future"
    Option -> "option"
    Fund   -> "fund"

instance FromJSON SymbolType where
  parseJSON = withText "SymbolType" $ \case
    "stock"  -> pure Stock
    "index"  -> pure Index
    "etf"    -> pure Etf
    "future" -> pure Future
    "option" -> pure Option
    "fund"   -> pure Fund
    other    -> fail $ "Unknown SymbolType: " <> (unpack $ show other)

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

fetchSymbols :: (?context :: context, LoggingProvider context) => SymbolType -> IO [Symbol]
fetchSymbols symbolType = do
  logInfo $ ("[fetchSymbols] begin | parameter : " <> tshow symbolType :: Text)
  result <- runPython 10000 "Web/Fetcher/symbol_fetcher.py" symbolType False :: IO (Either PythonError [Symbol])
  case result of
    Right symbols -> do
      logInfo $ "[fetchSymbols] end | nums - " <> tshow (length symbols)
      return symbols
    Left err -> do
      logError $ "[fetchSymbols] python error : " <> renderPythonError err
      fail (Text.unpack (renderPythonError err))
