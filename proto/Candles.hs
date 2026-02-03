module Proto.Candles where

import           Prelude
import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | Single candle formatted for lightweight-charts
-- time: ISO8601 string (or epoch seconds if you prefer); keep Text to stay agnostic.
data Candle = Candle
  { time   :: Text
  , open   :: Float
  , high   :: Float
  , low    :: Float
  , close  :: Float
  , volume :: Maybe Float
  , amount :: Maybe Float
  } deriving (Show, Eq, Generic)

instance ToJSON Candle
instance FromJSON Candle

-- | Response payload for /api/candles
data CandlesResponse = CandlesResponse
  { complete :: Bool
  , symbolType :: Text
  , symbolCode :: Text
  , timeframe  :: Int
  , dataPoints :: [Candle]
  } deriving (Show, Eq, Generic)

instance ToJSON CandlesResponse
instance FromJSON CandlesResponse
