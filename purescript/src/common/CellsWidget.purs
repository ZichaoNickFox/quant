module Common.CellsWidget
  ( buildCellsReadUrl
  , buildCellsReadUrlWithSeq
  , CellRow
  , CellUpdateInput
  , createFRP
  , decodeCellsFromJson
  , Events
  , selectedOwnerId
  , withNonce
  ) where

import Affjax.RequestBody as RB

import Affjax.ResponseFormat as RF
import Affjax.Web as AX
import Common.CellWidget as CellWidget
import Common.TreeNodeWidget as TreeNodeWidget
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FormURLEncoded as FUE
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
import FRP.Component.List as ListWidget
import Prelude
import Web.DOM.Document (createElement)
import Web.DOM.Element (setAttribute, toEventTarget, toNode)
import Web.DOM.Node (appendChild, setTextContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
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
  createRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] create req")
    ("[Cells " <> config.ownerType <> "] create resp")
    \ownerId -> do
      _ <- liftEffect do
        n <- Ref.read mutationSeqRef
        let n' = n + 1
        Ref.write n' mutationSeqRef
        pure n'
      requestCreate config.ownerType ownerId
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
  _ <- case mList of
    Nothing -> throw "CellsWidget: missing element for selector: [data-cell-list]"
    Just el -> pure el
  ensureInsertCellButton config.ownerType \ownerId ->
    createRequester.requestPush ownerId
  setInsertCellButtonVisible config.ownerType false
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
    Ref.write (map (\oid -> { ownerId: oid, name: "" }) ownerId) currentNodeRef
    case ownerId of
      Nothing -> do
        setCreateOwnerId ""
        listHandle.setItemsPush []
        setCellTitleVisible true
        setInsertCellButtonVisible config.ownerType false
        renderedPush unit
      Just _ ->
        reloadCells currentNodeRef requestRequester.requestPush

  _ <- FRP.subscribe requestRequester.responseEvent \(Tuple payload cells) -> do
    setCreateOwnerId payload.ownerId
    listHandle.setItemsPush cells
    when (payload.name /= "") do
      renderCellTitle payload.name
    setCellTitleVisible true
    setInsertCellButtonVisible config.ownerType (cells == [])
    renderedPush unit

  _ <- FRP.subscribe createRequester.responseEvent \created -> when created do
    mutatedPush unit
    reloadCells currentNodeRef requestRequester.requestPush

  _ <- FRP.subscribe createAtRequester.responseEvent \created -> when created do
    mutatedPush unit
    reloadCells currentNodeRef requestRequester.requestPush

  _ <- FRP.subscribe updateRequester.responseEvent \updated -> when updated do
    mutatedPush unit
    reloadCells currentNodeRef requestRequester.requestPush

  _ <- FRP.subscribe deleteRequester.responseEvent \deleted -> when deleted do
    mutatedPush unit
    reloadCells currentNodeRef requestRequester.requestPush

  _ <- FRP.subscribe moveRequester.responseEvent \moved -> when moved do
    mutatedPush unit
    reloadCells currentNodeRef requestRequester.requestPush

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

requestCreate :: String -> String -> Aff Boolean
requestCreate ownerType ownerId = do
  let body = FUE.fromArray
        [ Tuple "ownerType" (Just ownerType)
        , Tuple "ownerId" (Just ownerId)
        ]
  res <- AX.post_ "/CellCreate" (Just (RB.formURLEncoded body))
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

setCellTitleVisible :: Boolean -> Effect Unit
setCellTitleVisible visible = do
  win <- window
  doc <- document win
  mTitle <- querySelector (QuerySelector "[data-cell-title]") (HTMLDoc.toParentNode doc)
  for_ mTitle \el ->
    setAttribute "style" (if visible then "display:block;" else "display:none;") el

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

selectedOwnerId :: Maybe TreeNodeWidget.TreeNodePayload -> Maybe String
selectedOwnerId mbNode = map _.ownerId mbNode

ensureInsertCellButton :: String -> (String -> Effect Unit) -> Effect Unit
ensureInsertCellButton ownerType onCreate = do
  win <- window
  doc <- document win
  let root = HTMLDoc.toParentNode doc
  let selector = "[data-cell-create][data-owner-type='" <> ownerType <> "']"
  mCreateWrap <- querySelector (QuerySelector selector) root
  for_ mCreateWrap \createWrap -> do
    let btnSelector = "[data-cell-insert-first='1'][data-owner-type='" <> ownerType <> "']"
    mBtn <- querySelector (QuerySelector btnSelector) root
    case mBtn of
      Just _ -> pure unit
      Nothing -> do
        btn <- createElement "button" (HTMLDoc.toDocument doc)
        setAttribute "type" "button" btn
        setAttribute "class" "btn btn-sm btn-outline-secondary mb-2" btn
        setAttribute "data-cell-insert-first" "1" btn
        setAttribute "data-owner-type" ownerType btn
        setAttribute "style" "display:none;" btn
        setTextContent "Insert Cell" (toNode btn)
        onClick <- eventListener \_ -> do
          ownerId <- readCreateOwnerId
          when (ownerId /= "") (onCreate ownerId)
        addEventListener (EventType "click") onClick false (toEventTarget btn)
        _ <- appendChild (toNode btn) (toNode createWrap)
        pure unit

setInsertCellButtonVisible :: String -> Boolean -> Effect Unit
setInsertCellButtonVisible ownerType visible = do
  win <- window
  doc <- document win
  let root = HTMLDoc.toParentNode doc
  let selector = "[data-cell-insert-first='1'][data-owner-type='" <> ownerType <> "']"
  mBtn <- querySelector (QuerySelector selector) root
  for_ mBtn \btn ->
    setAttribute "style" (if visible then "display:inline-block;" else "display:none;") btn

reloadCells
  :: Ref.Ref (Maybe TreeNodeWidget.TreeNodePayload)
  -> (TreeNodeWidget.TreeNodePayload -> Effect Unit)
  -> Effect Unit
reloadCells currentNodeRef requestByNode = do
  mbNode <- Ref.read currentNodeRef
  for_ mbNode requestByNode
