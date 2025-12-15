module Web.Controller.StaticController where

import Web.Prelude
import Web.Types
import Web.View.Static.Index

instance Controller StaticController where
  action StaticAction = do
    render IndexView