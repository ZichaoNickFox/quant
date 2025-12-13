module Web.Controller.BacktestController where

import IHP.ControllerPrelude
import Web.View.Backtest.Index
import Web.Types

instance Controller BacktestController where
  action BacktestAction = do
    render IndexView