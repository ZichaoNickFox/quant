module Web.Controller.NoteController where

import IHP.ControllerPrelude
import Web.View.Note.Index
import Web.Types

instance Controller NoteController where
  action NoteAction = do
    render IndexView