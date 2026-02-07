module FRP.Component.Input
  ( InputAction(..)
  , InputOutput(..)
  , deriveInputOutput
  , createInput
  ) where

import Prelude

import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import FRP.Event (Event, create, mapAccum, subscribe)

data InputAction
  = StartEdit String
  | ConfirmEdit String

derive instance eqInputAction :: Eq InputAction
instance showInputAction :: Show InputAction where
  show = case _ of
    StartEdit v -> "(StartEdit " <> show v <> ")"
    ConfirmEdit v -> "(ConfirmEdit " <> show v <> ")"

data InputOutput
  = StartedEditing String
  | ConfirmedEditing String

derive instance eqInputOutput :: Eq InputOutput
instance showInputOutput :: Show InputOutput where
  show = case _ of
    StartedEditing v -> "(StartedEditing " <> show v <> ")"
    ConfirmedEditing v -> "(ConfirmedEditing " <> show v <> ")"

deriveInputOutput :: Event InputAction -> String -> Event InputOutput
deriveInputOutput actionEvent initialValue =
  let
    step action _ =
      case action of
        StartEdit v -> Tuple v (Just (StartedEditing v))
        ConfirmEdit v -> Tuple v (Just (ConfirmedEditing v))
  in
    filterMap identity $ mapAccum step actionEvent initialValue

createInput
  :: String
  -> Effect
       { actionPush :: InputAction -> Effect Unit
       , startedEvent :: Event String
       , confirmedEvent :: Event String
       }
createInput initialValue = do
  { event: actionEvent, push: actionPush } <- create
  { event: startedEvent, push: startedPush } <- create
  { event: confirmedEvent, push: confirmedPush } <- create
  let outputEvent = deriveInputOutput actionEvent initialValue
  _ <- subscribe outputEvent \out ->
    case out of
      StartedEditing v -> startedPush v
      ConfirmedEditing v -> confirmedPush v
  pure { actionPush, startedEvent, confirmedEvent }
