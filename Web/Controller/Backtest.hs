module Web.Controller.Backtest where

import IHP.ControllerPrelude
import Web.View.Backtest.Index
import Web.Types

instance Controller BacktestController where
  action BacktestAction = do
    render IndexView