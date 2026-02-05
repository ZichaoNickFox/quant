module Common.CellsWidget
  ( CellRow
  , CellUpdateInput
  , CellsUpdate(..)
  , CellsWidgetAction(..)
  , CellsWidgetHandle
  , createCellsWidgetByOwnerType
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
import Effect.Ref as Ref
import FRP as FRP
import FRP.Tree as Tree
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

data CellsUpdate
  = CellsRendered
  | CellsMutated

type CellsWidgetHandle =
  { onCellsUpdate :: FRP.Event CellsUpdate
  , actionPush :: CellsWidgetAction -> Effect Unit
  }

data CellsWidgetAction
  = LoadByTreeNode Tree.TreeNode
  | NotifyCellsMutated
  | SaveCell CellUpdateInput
  | DeleteCell String
  | InsertCellAbove String String
  | InsertCellBelow String String
  | MoveCellUp String
  | MoveCellDown String

createCellsWidgetByOwnerType
  :: { ownerType :: String
     , detailTitlePrefix :: String
     }
  -> Effect CellsWidgetHandle
createCellsWidgetByOwnerType config = do
  { event: actionEvent, push: actionPush } <- FRP.create
  { event: onCellsUpdate, push: cellsUpdatePush } <- FRP.create
  currentNodeRef <- Ref.new (Nothing :: Maybe Tree.TreeNode)
  refreshSeqRef <- Ref.new 0
  mutationSeqRef <- Ref.new 0
  cellWidget <- CellWidget.createCellWidget
  fetchRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] fetch req")
    ("[Cells " <> config.ownerType <> "] fetch resp")
    \node -> do
      seq <- liftEffect do
        n <- Ref.read refreshSeqRef
        let n' = n + 1
        Ref.write n' refreshSeqRef
        pure n'
      cells <- fetchCells config.ownerType node.id seq
      pure (Tuple node cells)
  fallbackFetchRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] fallback fetch req")
    ("[Cells " <> config.ownerType <> "] fallback fetch resp")
    \ownerId -> do
      seq <- liftEffect do
        n <- Ref.read refreshSeqRef
        let n' = n + 1
        Ref.write n' refreshSeqRef
        pure n'
      fetchCells config.ownerType ownerId seq
  createAtRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] create-at req")
    ("[Cells " <> config.ownerType <> "] create-at resp")
    \input -> do
      _ <- liftEffect do
        n <- Ref.read mutationSeqRef
        let n' = n + 1
        Ref.write n' mutationSeqRef
        pure n'
      callCreateAtAff config.ownerType input
  updateRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] update req")
    ("[Cells " <> config.ownerType <> "] update resp")
    \input -> do
      _ <- liftEffect do
        n <- Ref.read mutationSeqRef
        let n' = n + 1
        Ref.write n' mutationSeqRef
        pure n'
      callUpdateAff input
  deleteRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] delete req")
    ("[Cells " <> config.ownerType <> "] delete resp")
    \cellId -> do
      _ <- liftEffect do
        n <- Ref.read mutationSeqRef
        let n' = n + 1
        Ref.write n' mutationSeqRef
        pure n'
      callDeleteAff cellId
  moveRequester <- FRP.createRequester
    ("[Cells " <> config.ownerType <> "] move req")
    ("[Cells " <> config.ownerType <> "] move resp")
    \input -> do
      _ <- liftEffect do
        n <- Ref.read mutationSeqRef
        let n' = n + 1
        Ref.write n' mutationSeqRef
        pure n'
      callMoveAff input
  cellWidget.bindCellInteractions config.ownerType readCreateOwnerId

  _ <- FRP.subscribe cellWidget.onCellAction \cellAction ->
    case cellAction of
      CellWidget.Save input ->
        actionPush (SaveCell input)
      CellWidget.Delete cellId ->
        actionPush (DeleteCell cellId)
      CellWidget.InsertAbove ownerId cellId ->
        actionPush (InsertCellAbove ownerId cellId)
      CellWidget.InsertBelow ownerId cellId ->
        actionPush (InsertCellBelow ownerId cellId)
      CellWidget.MoveUp cellId ->
        actionPush (MoveCellUp cellId)
      CellWidget.MoveDown cellId ->
        actionPush (MoveCellDown cellId)

  _ <- FRP.subscribe actionEvent \action ->
    case action of
      NotifyCellsMutated ->
        do
          cellsUpdatePush CellsMutated
          reloadCells config.ownerType currentNodeRef fetchRequester.requestPush fallbackFetchRequester.requestPush
      LoadByTreeNode node ->
        do
          Ref.write (Just node) currentNodeRef
          fetchRequester.requestPush node
      SaveCell input ->
        updateRequester.requestPush input
      DeleteCell cellId ->
        deleteRequester.requestPush cellId
      InsertCellAbove ownerId anchorCellId ->
        createAtRequester.requestPush { ownerId, anchorCellId, position: "above" }
      InsertCellBelow ownerId anchorCellId ->
        createAtRequester.requestPush { ownerId, anchorCellId, position: "below" }
      MoveCellUp cellId ->
        moveRequester.requestPush { cellId, direction: "up" }
      MoveCellDown cellId ->
        moveRequester.requestPush { cellId, direction: "down" }

  _ <- FRP.subscribe fetchRequester.responseEvent \(Tuple node cells) -> do
    setCreateOwnerId node.id
    renderCellTitle (config.detailTitlePrefix <> node.name)
    CellWidget.renderCellList config.ownerType cells
    cellWidget.bindCellInteractions config.ownerType readCreateOwnerId
    cellsUpdatePush CellsRendered

  _ <- FRP.subscribe fallbackFetchRequester.responseEvent \cells -> do
    CellWidget.renderCellList config.ownerType cells
    cellWidget.bindCellInteractions config.ownerType readCreateOwnerId
    cellsUpdatePush CellsRendered

  _ <- FRP.subscribe createAtRequester.responseEvent \created -> when created do
    cellsUpdatePush CellsMutated
    reloadCells config.ownerType currentNodeRef fetchRequester.requestPush fallbackFetchRequester.requestPush

  _ <- FRP.subscribe updateRequester.responseEvent \updated -> when updated do
    cellsUpdatePush CellsMutated
    reloadCells config.ownerType currentNodeRef fetchRequester.requestPush fallbackFetchRequester.requestPush

  _ <- FRP.subscribe deleteRequester.responseEvent \deleted -> when deleted do
    cellsUpdatePush CellsMutated
    reloadCells config.ownerType currentNodeRef fetchRequester.requestPush fallbackFetchRequester.requestPush

  _ <- FRP.subscribe moveRequester.responseEvent \moved -> when moved do
    cellsUpdatePush CellsMutated
    reloadCells config.ownerType currentNodeRef fetchRequester.requestPush fallbackFetchRequester.requestPush

  pure
    { onCellsUpdate
    , actionPush
    }

callCreateAtAff :: String -> CellInsertInput -> Aff Boolean
callCreateAtAff ownerType input = do
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

callUpdateAff :: CellUpdateInput -> Aff Boolean
callUpdateAff input = do
  let body = FUE.fromArray
        [ Tuple "cellId" (Just input.cellId)
        , Tuple "cellType" (Just input.cellType)
        , Tuple "content" (Just input.content)
        ]
  res <- AX.post_ "/CellUpdate" (Just (RB.formURLEncoded body))
  pure case res of
    Left _ -> false
    Right _ -> true

callDeleteAff :: String -> Aff Boolean
callDeleteAff cellId = do
  let body = FUE.fromArray [ Tuple "cellId" (Just cellId) ]
  res <- AX.post_ "/CellDelete" (Just (RB.formURLEncoded body))
  pure case res of
    Left _ -> false
    Right _ -> true

callMoveAff :: CellMoveInput -> Aff Boolean
callMoveAff input = do
  let body = FUE.fromArray
        [ Tuple "cellId" (Just input.cellId)
        , Tuple "direction" (Just input.direction)
        ]
  res <- AX.post_ "/CellMove" (Just (RB.formURLEncoded body))
  pure case res of
    Left _ -> false
    Right _ -> true

fetchCells :: String -> String -> Int -> Aff (Array CellRow)
fetchCells ownerType ownerId seq = do
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
  -> Ref.Ref (Maybe Tree.TreeNode)
  -> (Tree.TreeNode -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit
reloadCells _ownerType currentNodeRef fetchByNode fetchByOwnerId = do
  mbNode <- Ref.read currentNodeRef
  case mbNode of
    Just node -> fetchByNode node
    Nothing -> do
      ownerId <- readCreateOwnerId
      when (ownerId /= "") (fetchByOwnerId ownerId)
