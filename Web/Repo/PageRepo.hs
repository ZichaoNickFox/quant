module Web.Repo.PageRepo
  ( getOrCreateNote
  , getOrCreateStrategy
  ) where

import Web.Prelude
import Web.Types

getOrCreateNote :: (?modelContext :: ModelContext) => IO Note
getOrCreateNote = do
  mb <- query @Note |> fetchOneOrNothing
  case mb of
    Just note -> pure note
    Nothing ->
      newRecord @Note
        |> set #name ("Untitled" :: Text)
        |> createRecord

getOrCreateStrategy :: (?modelContext :: ModelContext) => IO Strategy
getOrCreateStrategy = do
  mb <- query @Strategy |> fetchOneOrNothing
  case mb of
    Just strategy -> pure strategy
    Nothing ->
      newRecord @Strategy
        |> set #name ("Untitled" :: Text)
        |> createRecord
