module Proto.Symbols where

import           Prelude
import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Map     (Map)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | Response payload for /api/symbols
-- counts: key is symbolType (e.g. "stock", "index", ...), value is total rows
-- complete: True when server believes data already complete; False means a
--           background job was kicked off and client should re-fetch when notified.
data SymbolCountsResponse = SymbolCountsResponse
  { complete :: Bool
  , counts   :: Map Text Int
  } deriving (Show, Eq, Generic)

instance ToJSON SymbolCountsResponse
instance FromJSON SymbolCountsResponse
