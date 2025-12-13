module Web.Types where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Generated.Types
import IHP.Prelude
import IHP.ModelSupport
import IHP.Controller.Param (ParamReader(..))
import IHP.LoginSupport.Types

data QuantApplication = QuantApplication deriving (Eq, Show)

data StaticController = StaticAction deriving (Eq, Show, Data)

data DataController = DataAction
                    | DataActionGetSymbols deriving (Eq, Show, Data)

data NoteController = NoteAction deriving (Eq, Show, Data)

data StrategyController = StrategyAction deriving (Eq, Show, Data)

data BacktestController = BacktestAction deriving (Eq, Show, Data)