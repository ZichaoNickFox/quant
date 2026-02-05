module Web.Fetcher.FetchCandles
  ( CandleRange (..)
  , fetchCandles
  ) where

import           Control.Applicative          ((<|>))
import           Data.Aeson
import           Data.Text                      hiding (length)
import qualified Data.Text                      as Text
import           Data.Time
import           Data.UUID                      (UUID)
import           GHC.Generics
import           Web.Fetcher.Python
import           Web.Prelude
import           Web.Types

-- | Parameters sent to the Python downloader.
--   { "symbol_id": "...", "symbol_code": "...", "timeframe": 0-7, "from_datetime": "...", "to_datetime": "..." }
data CandleRange = CandleRange
  { symbolId     :: UUID
  , symbolCode   :: Text
  , timeframe    :: Int
  , fromDatetime :: Text
  , toDatetime   :: Text
  } deriving (Show, Generic)

instance ToJSON CandleRange where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON CandleRange where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- | Decode python JSON directly into DB Candle (no DTO).
instance FromJSON Candle where
  parseJSON = withObject "Candle" $ \o -> do
    symbolId <- o .: "symbol_id"
    timeframe <- o .: "timeframe"
    datetimeTxt <- o .: "datetime"
    open <- o .: "open"
    close <- o .: "close"
    high <- o .: "high"
    low <- o .: "low"
    volume <- o .: "volume"
    amount <- o .: "amount"
    datetime <-
      parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" datetimeTxt
        <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" datetimeTxt
    pure $ newRecord @Candle
      |> set #symbolId symbolId
      |> set #timeframe timeframe
      |> set #datetime datetime
      |> set #open open
      |> set #close close
      |> set #high high
      |> set #low low
      |> set #volume volume
      |> set #amount amount

fetchCandles
  :: (?context :: context, LoggingProvider context)
  => CandleRange -> IO [Candle]
fetchCandles candleRange = do
  logFetch $ ("[fetchCandles] begin | parameter : " <> tshow candleRange :: Text)
  result <- runPython 10000 "Web/Fetcher/fetch_candles.py" candleRange False :: IO (Either PythonError [Candle])
  case result of
    Right candles -> do
      logFetch $ "[fetchCandles] end | nums - " <> tshow (length candles)
      pure candles
    Left err -> do
      logError $ "[fetchCandles] python error : " <> renderPythonError err
      fail (Text.unpack (renderPythonError err))
