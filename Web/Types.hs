module Web.Types
  ( module Generated.Enums
  , module Generated.Types
  , module Web.Types
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import           Data.Data
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Generated.Enums
import           Generated.Types
import           IHP.RouterPrelude (endOfInput, string)
import           Web.Prelude

-- Application
data WebApplication = WebApplication deriving (Eq, Show)

-- Controller
data ColdWarmController = APICandlesAction | APISymbolsAction deriving (Eq, Show, Data)
data PageController
  = PageDataAction
  | PageStrategyCreateAction
  | PageStrategyAction
  | PageBacktestAction
  | PageRuntimeAction
  deriving (Eq, Show, Data)
data CellController
  = CellCreateAction
  | CellCreateAtAction
  | CellUpdateAction
  | CellMoveAction
  | CellDeleteAction
  | CellReadAction
  deriving (Eq, Show, Data)
data StrategyController
  = StrategyCreateAction
  | StrategyTreeCreateAction
  | StrategyTreeDeleteAction
  | StrategyTreeReadAction
  | StrategyTreeUpdateAction
  | StrategyReadAction
  | StrategyUpdateAction
  | StrategyDeleteAction
  deriving (Eq, Show, Data)
data BacktestController
  = BacktestRunAction
  deriving (Eq, Show, Data)
data NotifyController = NotifyAction deriving (Eq, Show, Data)

-- Route
instance AutoRoute ColdWarmController
instance AutoRoute PageController
instance AutoRoute CellController
instance AutoRoute StrategyController
instance AutoRoute BacktestController
instance CanRoute NotifyController where
  parseRoute' = string "/sse/notify" <* endOfInput >> pure NotifyAction
instance HasPath NotifyController where
  pathTo NotifyAction = "/sse/notify"

-- Web
data SelectedSymbol = SelectedSymbol
  { symbolType :: SymbolType
  , symbolCode :: Text
  } deriving (Eq, Show)

type TypeSymbolsMap = Map SymbolType [Symbol]
