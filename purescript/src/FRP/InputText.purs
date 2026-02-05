module FRP.InputText
  ( InputTextAction(..)
  , InputTextOutput(..)
  , deriveInputTextOutput
  , createInputText
  ) where

import Prelude

import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import FRP.Event (Event, create, mapAccum, subscribe)

data InputTextAction
  = StartEdit String
  | ConfirmEdit String

derive instance eqInputTextAction :: Eq InputTextAction
instance showInputTextAction :: Show InputTextAction where
  show = case _ of
    StartEdit v -> "(StartEdit " <> show v <> ")"
    ConfirmEdit v -> "(ConfirmEdit " <> show v <> ")"

data InputTextOutput
  = StartedEditing String
  | ConfirmedEditing String

derive instance eqInputTextOutput :: Eq InputTextOutput
instance showInputTextOutput :: Show InputTextOutput where
  show = case _ of
    StartedEditing v -> "(StartedEditing " <> show v <> ")"
    ConfirmedEditing v -> "(ConfirmedEditing " <> show v <> ")"

deriveInputTextOutput :: Event InputTextAction -> String -> Event InputTextOutput
deriveInputTextOutput actionEvent initialValue =
  let
    step action _ =
      case action of
        StartEdit v -> Tuple v (Just (StartedEditing v))
        ConfirmEdit v -> Tuple v (Just (ConfirmedEditing v))
  in
    filterMap identity $ mapAccum step actionEvent initialValue

createInputText
  :: String
  -> Effect
       { actionPush :: InputTextAction -> Effect Unit
       , startedEvent :: Event String
       , confirmedEvent :: Event String
       }
createInputText initialValue = do
  { event: actionEvent, push: actionPush } <- create
  { event: startedEvent, push: startedPush } <- create
  { event: confirmedEvent, push: confirmedPush } <- create
  let outputEvent = deriveInputTextOutput actionEvent initialValue
  _ <- subscribe outputEvent \out ->
    case out of
      StartedEditing v -> startedPush v
      ConfirmedEditing v -> confirmedPush v
  pure { actionPush, startedEvent, confirmedEvent }
