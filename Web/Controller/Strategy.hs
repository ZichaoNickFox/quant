module Web.Controller.Strategy where

import IHP.ControllerPrelude
import Web.View.Strategy.Index
import Web.Types

instance Controller StrategyController where
  action StrategyAction = do
    render IndexView