module Proto.Strategy where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Prelude

data StrategyInfo = StrategyInfo
  { id   :: Text
  , name :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON StrategyInfo
instance FromJSON StrategyInfo
