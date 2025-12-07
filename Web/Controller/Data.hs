module Web.Controller.Data where

import Web.Controller.Prelude
import Web.View.Data.Index

instance Controller DataController where
    action DataHomeAction = do
        render IndexView