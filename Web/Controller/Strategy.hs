module Web.Controller.Strategy where

import Web.Controller.Prelude
import Web.View.Strategy.Index

instance Controller StrategyController where
  action StrategyAction = do
      render IndexView