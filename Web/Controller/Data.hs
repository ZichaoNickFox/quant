module Web.Controller.Data where

import IHP.ControllerPrelude
import Web.View.Data.Index
import Web.Types

instance Controller DataController where
  action DataAction = do
    render IndexView