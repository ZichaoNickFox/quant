module Common.CellsWidget
  ( CellRow
  , CellUpdateInput
  , Events
  , createFRP
  , buildCellsReadUrl
  , buildCellsReadUrlWithSeq
  , withNonce
  , decodeCellsFromJson
  ) where

import Prelude

import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.Web as AX
import Common.CellWidget as CellWidget
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.FormURLEncoded as FUE
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import FRP as FRP
import Common.TreeNodeWidget as TreeNodeWidget
import FRP.Component.Tree as Tree
import FRP.Component.List as ListWidget
import Web.DOM.Element (toNode)
import Web.DOM.Node (setTextContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.HTMLInputElement as HTMLInput
import Web.HTML.Window (document)

type CellRow = CellWidget.CellRow

type CellUpdateInput = CellWidget.CellUpdateInput

type CellInsertInput =
  { ownerId :: String
  , anchorCellId :: String
  , position :: String
  }

type CellMoveInput =
  { cellId :: String
  , direction :: String
  }

type Events =
  { loadByOwnerIdPush :: Maybe String -> Effect Unit
  , onRenderedSubs :: FRP.Event Unit
  , onMutatedSubs :: FRP.Event Unit
  }

createFRP
  :: { ownerType :: String
     , detailTitlePrefix :: String
     }
  -> Effect Events
createFRP config = do
  { event: loadByOwnerIdEvent, push: loadByOwnerIdPush } <- FRP.create
  { event: onRenderedSubs, push: renderedPush } <- FRP.create
  { event: onMutatedSubs, push: mutatedPush } <- FRP.create
  currentNodeRef <- Ref.new (Nothing :: Maybe TreeNodeWidget.TreeNodePayload)
  refreshSeqRef <- Ref.new 0
  mutationSeqRef <- Ref.new 0
  requestRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] request req")
    ("[Cells " <> config.ownerType <> "] request resp")
    \payload -> do
      seq <- liftEffect do
        n <- Ref.read refreshSeqRef
        let n' = n + 1
        Ref.write n' refreshSeqRef
        pure n'
      cells <- requestCells config.ownerType payload.ownerId seq
      pure (Tuple payload cells)
  fallbackRequestRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] fallback request req")
    ("[Cells " <> config.ownerType <> "] fallback request resp")
    \ownerId -> do
      seq <- liftEffect do
        n <- Ref.read refreshSeqRef
        let n' = n + 1
        Ref.write n' refreshSeqRef
        pure n'
      requestCells config.ownerType ownerId seq
  createAtRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] create-at req")
    ("[Cells " <> config.ownerType <> "] create-at resp")
    \input -> do
      _ <- liftEffect do
        n <- Ref.read mutationSeqRef
        let n' = n + 1
        Ref.write n' mutationSeqRef
        pure n'
      requestCreateAt config.ownerType input
  updateRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] update req")
    ("[Cells " <> config.ownerType <> "] update resp")
    \input -> do
      _ <- liftEffect do
        n <- Ref.read mutationSeqRef
        let n' = n + 1
        Ref.write n' mutationSeqRef
        pure n'
      requestUpdate input
  deleteRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] delete req")
    ("[Cells " <> config.ownerType <> "] delete resp")
    \cellId -> do
      _ <- liftEffect do
        n <- Ref.read mutationSeqRef
        let n' = n + 1
        Ref.write n' mutationSeqRef
        pure n'
      requestDelete cellId
  moveRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] move req")
    ("[Cells " <> config.ownerType <> "] move resp")
    \input -> do
      _ <- liftEffect do
        n <- Ref.read mutationSeqRef
        let n' = n + 1
        Ref.write n' mutationSeqRef
        pure n'
      requestMove input
  win <- window
  doc <- document win
  let doc' = HTMLDoc.toDocument doc
  mList <- querySelector (QuerySelector "[data-cell-list]") (HTMLDoc.toParentNode doc)
  listEl <- case mList of
    Nothing -> throw "CellsWidget: missing element for selector: [data-cell-list]"
    Just el -> pure el
  listHandle <- ListWidget.createFRP
    { getItemId: \cell -> cell.id
    , createItemFRP: CellWidget.createFRP doc' config.ownerType readCreateOwnerId
    }

  _ <- FRP.subscribe listHandle.onItemCreatedSubs \events -> do
    _ <- FRP.subscribe events.onSaveSubs updateRequester.requestPush
    _ <- FRP.subscribe events.onDeleteSubs deleteRequester.requestPush
    _ <- FRP.subscribe events.onInsertAboveSubs \payload ->
      createAtRequester.requestPush
        { ownerId: payload.ownerId
        , anchorCellId: payload.cellId
        , position: "above"
            }
    _ <- FRP.subscribe events.onInsertBelowSubs \payload ->
      createAtRequester.requestPush
        { ownerId: payload.ownerId
        , anchorCellId: payload.cellId
        , position: "below"
        }
    _ <- FRP.subscribe events.onMoveUpSubs \cellId ->
      moveRequester.requestPush { cellId, direction: "up" }
    _ <- FRP.subscribe events.onMoveDownSubs \cellId ->
      moveRequester.requestPush { cellId, direction: "down" }
    pure unit

  _ <- FRP.subscribe loadByOwnerIdEvent \ownerId -> do
    for_ ownerId \oid ->
      Ref.write (Just { ownerId: oid, name: "" }) currentNodeRef
    reloadCells config.ownerType currentNodeRef requestRequester.requestPush fallbackRequestRequester.requestPush

  _ <- FRP.subscribe requestRequester.responseEvent \(Tuple payload cells) -> do
    setCreateOwnerId payload.ownerId
    renderCellTitle (config.detailTitlePrefix <> payload.name)
    listHandle.setItemsPush cells
    renderedPush unit

  _ <- FRP.subscribe fallbackRequestRequester.responseEvent \cells -> do
    listHandle.setItemsPush cells
    renderedPush unit

  _ <- FRP.subscribe createAtRequester.responseEvent \created -> when created do
    mutatedPush unit
    reloadCells config.ownerType currentNodeRef requestRequester.requestPush fallbackRequestRequester.requestPush

  _ <- FRP.subscribe updateRequester.responseEvent \updated -> when updated do
    mutatedPush unit
    reloadCells config.ownerType currentNodeRef requestRequester.requestPush fallbackRequestRequester.requestPush

  _ <- FRP.subscribe deleteRequester.responseEvent \deleted -> when deleted do
    mutatedPush unit
    reloadCells config.ownerType currentNodeRef requestRequester.requestPush fallbackRequestRequester.requestPush

  _ <- FRP.subscribe moveRequester.responseEvent \moved -> when moved do
    mutatedPush unit
    reloadCells config.ownerType currentNodeRef requestRequester.requestPush fallbackRequestRequester.requestPush

  pure
    { loadByOwnerIdPush
    , onRenderedSubs
    , onMutatedSubs
    }

requestCreateAt :: String -> CellInsertInput -> Aff Boolean
requestCreateAt ownerType input = do
  let body = FUE.fromArray
        [ Tuple "ownerType" (Just ownerType)
        , Tuple "ownerId" (Just input.ownerId)
        , Tuple "anchorCellId" (Just input.anchorCellId)
        , Tuple "position" (Just input.position)
        ]
  res <- AX.post_ "/CellCreateAt" (Just (RB.formURLEncoded body))
  pure case res of
    Left _ -> false
    Right _ -> true

requestUpdate :: CellUpdateInput -> Aff Boolean
requestUpdate input = do
  let body = FUE.fromArray
        [ Tuple "cellId" (Just input.cellId)
        , Tuple "cellType" (Just input.cellType)
        , Tuple "content" (Just input.content)
        ]
  res <- AX.post_ "/CellUpdate" (Just (RB.formURLEncoded body))
  pure case res of
    Left _ -> false
    Right _ -> true

requestDelete :: String -> Aff Boolean
requestDelete cellId = do
  let body = FUE.fromArray [ Tuple "cellId" (Just cellId) ]
  res <- AX.post_ "/CellDelete" (Just (RB.formURLEncoded body))
  pure case res of
    Left _ -> false
    Right _ -> true

requestMove :: CellMoveInput -> Aff Boolean
requestMove input = do
  let body = FUE.fromArray
        [ Tuple "cellId" (Just input.cellId)
        , Tuple "direction" (Just input.direction)
        ]
  res <- AX.post_ "/CellMove" (Just (RB.formURLEncoded body))
  pure case res of
    Left _ -> false
    Right _ -> true

requestCells :: String -> String -> Int -> Aff (Array CellRow)
requestCells ownerType ownerId seq = do
  let url = buildCellsReadUrlWithSeq ownerType ownerId seq
  response <- AX.get RF.json url
  pure $ case response of
    Left _ -> []
    Right ok -> CellWidget.decodeCellsFromJson ok.body

buildCellsReadUrl :: String -> String -> String
buildCellsReadUrl ownerType ownerId =
  "/CellRead?ownerType=" <> ownerType <> "&ownerId=" <> ownerId

buildCellsReadUrlWithSeq :: String -> String -> Int -> String
buildCellsReadUrlWithSeq ownerType ownerId seq =
  buildCellsReadUrl ownerType ownerId <> "&r=" <> show seq

decodeCellsFromJson :: J.Json -> Array CellRow
decodeCellsFromJson = CellWidget.decodeCellsFromJson

setCreateOwnerId :: String -> Effect Unit
setCreateOwnerId ownerId = do
  win <- window
  doc <- document win
  mInput <- querySelector (QuerySelector "[data-cell-create-owner-id]") (HTMLDoc.toParentNode doc)
  for_ mInput \el ->
    case HTMLInput.fromElement el of
      Nothing -> pure unit
      Just input -> HTMLInput.setValue ownerId input

renderCellTitle :: String -> Effect Unit
renderCellTitle title = do
  win <- window
  doc <- document win
  mTitle <- querySelector (QuerySelector "[data-cell-title]") (HTMLDoc.toParentNode doc)
  for_ mTitle \el ->
    setTextContent title (toNode el)

withNonce :: String -> Int -> String
withNonce url seq =
  let sep = if String.contains (Pattern "?") url then "&" else "?"
  in url <> sep <> "_r=" <> show seq

readCreateOwnerId :: Effect String
readCreateOwnerId = do
  win <- window
  doc <- document win
  mInput <- querySelector (QuerySelector "[data-cell-create-owner-id]") (HTMLDoc.toParentNode doc)
  case mInput >>= HTMLInput.fromElement of
    Just input -> HTMLInput.value input
    Nothing -> pure ""

reloadCells
  :: String
  -> Ref.Ref (Maybe TreeNodeWidget.TreeNodePayload)
  -> (TreeNodeWidget.TreeNodePayload -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit
reloadCells _ownerType currentNodeRef requestByNode requestByOwnerId = do
  mbNode <- Ref.read currentNodeRef
  case mbNode of
    Just node -> requestByNode node
    Nothing -> do
      ownerId <- readCreateOwnerId
      when (ownerId /= "") (requestByOwnerId ownerId)
