module Web.Controller.BacktestController where

import Web.Prelude
import Web.Types
import Web.View.Backtest.Index

instance Controller BacktestController where
  action BacktestAction = do
    render IndexView