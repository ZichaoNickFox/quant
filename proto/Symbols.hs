module Proto.Symbols where

import           Prelude
import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Map     (Map)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data SymbolInfo = SymbolInfo
  { symbolType :: Text
  , code       :: Text
  , name       :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON SymbolInfo
instance FromJSON SymbolInfo

-- | Response payload for /APISymbols
-- symbols: list of all symbols (client can group by symbolType)
-- complete: True when server believes data already complete; False means a
--           background job was kicked off and client should re-fetch when notified.
data APISymbolsResponse = APISymbolsResponse
  { complete :: Bool
  , symbols  :: [SymbolInfo]
  } deriving (Show, Eq, Generic)

instance ToJSON APISymbolsResponse
instance FromJSON APISymbolsResponse
