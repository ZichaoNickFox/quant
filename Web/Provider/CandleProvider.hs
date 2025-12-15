module Web.Provider.CandleProvider
  ( getCandles
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