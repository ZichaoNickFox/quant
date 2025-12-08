module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types
import IHP.LoginSupport.Types

data QuantApplication = QuantApplication deriving (Eq, Show)

data StaticController = StaticAction deriving (Eq, Show, Data)

data DataController = DataAction deriving (Eq, Show, Data)

data BacktestController = BacktestAction deriving (Eq, Show, Data)

data StrategyController = StrategyAction deriving (Eq, Show, Data)