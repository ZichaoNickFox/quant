module Web.Controller.Backtest where

import Web.Controller.Prelude
import Web.View.Backtest.Index

instance Controller BacktestController where
    action BacktestHomeAction = do
        render IndexView