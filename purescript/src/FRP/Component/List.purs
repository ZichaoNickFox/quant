module FRP.Component.List
  ( Config
  , Events
  , createFRP
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import FRP as FRP

type Config item externalId itemEvents =
  { getItemId :: item -> externalId  -- IMPORTANT: Must return unique id for each item
  , createItemFRP :: item -> Effect itemEvents
  }

type ListItem item externalId =
  { externalId :: externalId  -- External id (must be unique)
  , order :: Int
  , payload :: item
  }

type Events item externalId itemEvents =
  { setItemsPush :: Array item -> Effect Unit
  , onItemsChanged :: FRP.Event (Array item)
  , onItemCreatedSubs :: FRP.Event itemEvents
  , moveUpPush :: externalId -> Effect Unit    -- External id must be unique
  , moveDownPush :: externalId -> Effect Unit  -- External id must be unique
  }

createFRP :: forall item externalId itemEvents. Eq externalId => Config item externalId itemEvents -> Effect (Events item externalId itemEvents)
createFRP config = do
  { event: onItemsChanged, push: pushItemsChanged } <- FRP.create
  { event: onItemCreatedSubs, push: pushItemCreated } <- FRP.create
  itemsRef <- Ref.new ([] :: Array (ListItem item externalId))
  let setItems items = do
        let withIds = A.mapWithIndex
              (\index item ->
                { externalId: config.getItemId item  -- External id must be unique
                , payload: item
                , order: index
                })
              items
        Ref.write withIds itemsRef
        for_ withIds \item -> do
          itemEvents <- config.createItemFRP item.payload
          pushItemCreated itemEvents
        let sorted = A.sortBy (comparing _.order) withIds
        pushItemsChanged (map _.payload sorted)
      applyMove delta extId = do
        items <- Ref.read itemsRef
        let sorted = A.sortBy (comparing _.order) items
        case A.findIndex (\listItem -> listItem.externalId == extId) sorted of
          Nothing -> pure unit
          Just index -> do
            let target = index + delta
            if target < 0 || target >= A.length sorted then
              pure unit
            else
              case A.index sorted index, A.index sorted target of
                Just current, Just other -> do
                  let updated = map (\entry ->
                        if entry.externalId == current.externalId then entry { order = other.order }
                        else if entry.externalId == other.externalId then entry { order = current.order }
                        else entry
                        ) items
                  Ref.write updated itemsRef
                  let sortedUpdated = A.sortBy (comparing _.order) updated
                  pushItemsChanged (map _.payload sortedUpdated)
                _, _ -> pure unit
      moveUpItem extId = applyMove (-1) extId
      moveDownItem extId = applyMove 1 extId
  pure
    { setItemsPush: setItems
    , onItemsChanged
    , onItemCreatedSubs
    , moveUpPush: moveUpItem
    , moveDownPush: moveDownItem
    }
