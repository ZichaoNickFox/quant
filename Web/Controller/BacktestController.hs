module Web.Controller.BacktestController where

import qualified Data.Aeson as A
import Web.Prelude
import Web.Types

instance Controller BacktestController where
  action BacktestRunAction = do
    renderJson (A.object [ "ok" A..= True ])
