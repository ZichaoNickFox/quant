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
import           Web.Prelude

-- Application
data WebApplication = WebApplication deriving (Eq, Show)

-- Controller
data APIController = CandlesAction deriving (Eq, Show, Data)
data BacktestController = BacktestAction deriving (Eq, Show, Data)
data DataController = DataAction
                    | DataGetSymbolsAction deriving (Eq, Show, Data)
data NoteController = NoteAction deriving (Eq, Show, Data)
data RuntimeController = RuntimeAction deriving (Eq, Show, Data)
data StrategyController = StrategyAction deriving (Eq, Show, Data)
data StaticController = StaticAction deriving (Eq, Show, Data)

-- Route
instance AutoRoute APIController
instance AutoRoute BacktestController
instance AutoRoute DataController
instance AutoRoute NoteController
instance AutoRoute RuntimeController
instance AutoRoute StrategyController
instance AutoRoute StaticController

-- Web
data SelectedSymbol = SelectedSymbol
  { symbolType :: SymbolType
  , symbolCode :: Text
  } deriving (Eq, Show)

type TypeSymbolsMap = Map SymbolType [Symbol]