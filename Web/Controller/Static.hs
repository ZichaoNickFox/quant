module Web.Controller.Static where

import IHP.ControllerPrelude
import Web.View.Static.Index
import Web.Types

instance Controller StaticController where
  action StaticAction = do
    render IndexView