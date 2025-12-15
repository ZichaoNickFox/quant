module Web.Controller.NoteController where

import Web.Prelude
import Web.Types
import Web.View.Note.Index

instance Controller NoteController where
  action NoteAction = do
    render IndexView