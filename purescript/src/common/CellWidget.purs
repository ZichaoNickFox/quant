module Common.CellWidget
  ( CellRow
  , CellUpdateInput
  , CellWidgetAction(..)
  , CellWidgetHandle
  , createCellWidget
  , renderCellList
  , decodeCellsFromJson
  ) where

import Prelude

import Data.Argonaut.Core as J
import Data.Argonaut.Decode as D
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import FRP as FRP
import Foreign.Object as FO
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element (Element, fromNode, setAttribute, toEventTarget, toNode, toParentNode)
import Web.DOM.Node (appendChild, setTextContent)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.HTMLInputElement as HTMLInput
import Web.HTML.HTMLSelectElement as HTMLSelect
import Web.HTML.HTMLTextAreaElement as HTMLTextArea
import Web.HTML.Window (document)

type CellRow =
  { id :: String
  , cellType :: String
  , cellOrder :: Int
  , content :: Maybe String
  }

type CellUpdateInput =
  { cellId :: String
  , cellType :: String
  , content :: String
  }

data CellWidgetAction
  = Save CellUpdateInput
  | Delete String
  | InsertAbove String String
  | InsertBelow String String
  | MoveUp String
  | MoveDown String

type CellWidgetHandle =
  { onCellAction :: FRP.Event CellWidgetAction
  , bindCellInteractions :: String -> Effect String -> Effect Unit
  }

createCellWidget :: Effect CellWidgetHandle
createCellWidget = do
  { event: onCellAction, push } <- FRP.create
  pure
    { onCellAction
    , bindCellInteractions: \ownerType readOwnerId ->
        bindCellInteractionsInternal ownerType readOwnerId push
    }

bindCellInteractionsInternal :: String -> Effect String -> (CellWidgetAction -> Effect Unit) -> Effect Unit
bindCellInteractionsInternal ownerType readOwnerId push = do
  win <- window
  doc <- document win
  cards <- querySelectorAll (QuerySelector ("[data-cell-update-card][data-owner-type='" <> ownerType <> "']")) (HTMLDoc.toParentNode doc)
  cardNodes <- NodeList.toArray cards
  for_ (A.mapMaybe fromNode cardNodes) \cardEl -> do
    mCellId <- querySelector (QuerySelector "[name='cellId']") (toParentNode cardEl)
    let readCellId = case mCellId >>= HTMLInput.fromElement of
          Nothing -> pure ""
          Just cellIdInput -> HTMLInput.value cellIdInput

    mContent <- querySelector (QuerySelector "[name='content']") (toParentNode cardEl)
    for_ mContent \contentEl -> do
      listener <- eventListener \_ -> do
        mbInput <- readUpdateInputFromContainer cardEl
        for_ mbInput (push <<< Save)
      addEventListener (EventType "blur") listener false (toEventTarget contentEl)

    mDeleteBtn <- querySelector (QuerySelector "[data-cell-delete-button]") (toParentNode cardEl)
    for_ mDeleteBtn \deleteBtn -> do
      listener <- eventListener \_ -> do
        cellId <- readCellId
        when (cellId /= "") (push (Delete cellId))
      addEventListener (EventType "click") listener false (toEventTarget deleteBtn)

    mAddAbove <- querySelector (QuerySelector "[data-cell-add-above]") (toParentNode cardEl)
    for_ mAddAbove \btn -> do
      listener <- eventListener \_ -> do
        cellId <- readCellId
        ownerId <- readOwnerId
        when (cellId /= "" && ownerId /= "") (push (InsertAbove ownerId cellId))
      addEventListener (EventType "click") listener false (toEventTarget btn)

    mAddBelow <- querySelector (QuerySelector "[data-cell-add-below]") (toParentNode cardEl)
    for_ mAddBelow \btn -> do
      listener <- eventListener \_ -> do
        cellId <- readCellId
        ownerId <- readOwnerId
        when (cellId /= "" && ownerId /= "") (push (InsertBelow ownerId cellId))
      addEventListener (EventType "click") listener false (toEventTarget btn)

    mMoveUp <- querySelector (QuerySelector "[data-cell-move-up]") (toParentNode cardEl)
    for_ mMoveUp \btn -> do
      listener <- eventListener \_ -> do
        cellId <- readCellId
        when (cellId /= "") (push (MoveUp cellId))
      addEventListener (EventType "click") listener false (toEventTarget btn)

    mMoveDown <- querySelector (QuerySelector "[data-cell-move-down]") (toParentNode cardEl)
    for_ mMoveDown \btn -> do
      listener <- eventListener \_ -> do
        cellId <- readCellId
        when (cellId /= "") (push (MoveDown cellId))
      addEventListener (EventType "click") listener false (toEventTarget btn)

readUpdateInputFromContainer :: Element -> Effect (Maybe CellUpdateInput)
readUpdateInputFromContainer container = do
  mCellId <- querySelector (QuerySelector "[name='cellId']") (toParentNode container)
  mCellType <- querySelector (QuerySelector "[name='cellType']") (toParentNode container)
  mContent <- querySelector (QuerySelector "[name='content']") (toParentNode container)
  case Tuple mCellId mCellType of
    Tuple (Just cellIdEl) (Just cellTypeEl) -> do
      let mCellIdInput = HTMLInput.fromElement cellIdEl
      let mCellTypeSelect = HTMLSelect.fromElement cellTypeEl
      content <- case mContent >>= HTMLTextArea.fromElement of
        Just contentEl -> HTMLTextArea.value contentEl
        Nothing -> pure ""
      case Tuple mCellIdInput mCellTypeSelect of
        Tuple (Just cellIdInput) (Just cellTypeSelect) -> do
          cellId <- HTMLInput.value cellIdInput
          cellType <- HTMLSelect.value cellTypeSelect
          pure $ if cellId == ""
            then Nothing
            else Just { cellId, cellType, content }
        _ -> pure Nothing
    _ -> pure Nothing

renderCellList :: String -> Array CellRow -> Effect Unit
renderCellList ownerType cells = do
  win <- window
  doc <- document win
  mList <- querySelector (QuerySelector "[data-cell-list]") (HTMLDoc.toParentNode doc)
  for_ mList \listEl -> do
    setTextContent "" (toNode listEl)
    let doc' = HTMLDoc.toDocument doc
    traverse_ (appendCell doc' listEl ownerType) cells

appendCell :: Document -> Element -> String -> CellRow -> Effect Unit
appendCell doc listEl ownerType cell = do
  card <- createElement "div" doc
  setAttribute "class" "card mb-2" card
  body <- createElement "div" doc
  setAttribute "class" "card-body" body
  updateCard <- createElement "div" doc
  setAttribute "data-cell-update-card" "1" updateCard
  setAttribute "data-owner-type" ownerType updateCard
  idInput <- createElement "input" doc
  setAttribute "type" "hidden" idInput
  setAttribute "name" "cellId" idInput
  setAttribute "value" cell.id idInput
  typeSelect <- createElement "select" doc
  setAttribute "name" "cellType" typeSelect
  setAttribute "class" "form-select form-select-sm w-auto mb-2" typeSelect
  appendCellTypeOption doc typeSelect cell.cellType "raw" "Raw"
  appendCellTypeOption doc typeSelect cell.cellType "image" "Image"
  appendCellTypeOption doc typeSelect cell.cellType "backtest" "Backtest"
  content <- createElement "textarea" doc
  setAttribute "class" "form-control mb-2" content
  setAttribute "name" "content" content
  setAttribute "rows" "4" content
  case HTMLTextArea.fromElement content of
    Just textarea -> HTMLTextArea.setValue (fromMaybe "" cell.content) textarea
    Nothing -> setTextContent (fromMaybe "" cell.content) (toNode content)
  _ <- appendChild (toNode idInput) (toNode updateCard)
  _ <- appendChild (toNode typeSelect) (toNode updateCard)
  _ <- appendChild (toNode content) (toNode updateCard)

  delBtn <- createElement "button" doc
  setAttribute "type" "button" delBtn
  setAttribute "class" "btn btn-sm btn-outline-danger" delBtn
  setAttribute "data-cell-delete-button" "1" delBtn
  setTextContent "Delete" (toNode delBtn)
  actionRow <- createElement "div" doc
  setAttribute "class" "d-flex gap-2 flex-wrap mt-2" actionRow

  addAboveBtn <- createElement "button" doc
  setAttribute "type" "button" addAboveBtn
  setAttribute "class" "btn btn-sm btn-outline-secondary" addAboveBtn
  setAttribute "data-cell-add-above" "1" addAboveBtn
  setTextContent "Insert Above" (toNode addAboveBtn)

  addBelowBtn <- createElement "button" doc
  setAttribute "type" "button" addBelowBtn
  setAttribute "class" "btn btn-sm btn-outline-secondary" addBelowBtn
  setAttribute "data-cell-add-below" "1" addBelowBtn
  setTextContent "Insert Below" (toNode addBelowBtn)

  moveUpBtn <- createElement "button" doc
  setAttribute "type" "button" moveUpBtn
  setAttribute "class" "btn btn-sm btn-outline-secondary" moveUpBtn
  setAttribute "data-cell-move-up" "1" moveUpBtn
  setTextContent "Move Up" (toNode moveUpBtn)

  moveDownBtn <- createElement "button" doc
  setAttribute "type" "button" moveDownBtn
  setAttribute "class" "btn btn-sm btn-outline-secondary" moveDownBtn
  setAttribute "data-cell-move-down" "1" moveDownBtn
  setTextContent "Move Down" (toNode moveDownBtn)

  _ <- appendChild (toNode delBtn) (toNode actionRow)
  _ <- appendChild (toNode addAboveBtn) (toNode actionRow)
  _ <- appendChild (toNode addBelowBtn) (toNode actionRow)
  _ <- appendChild (toNode moveUpBtn) (toNode actionRow)
  _ <- appendChild (toNode moveDownBtn) (toNode actionRow)

  _ <- appendChild (toNode actionRow) (toNode updateCard)
  _ <- appendChild (toNode updateCard) (toNode body)
  _ <- appendChild (toNode body) (toNode card)
  _ <- appendChild (toNode card) (toNode listEl)
  pure unit

appendCellTypeOption :: Document -> Element -> String -> String -> String -> Effect Unit
appendCellTypeOption doc selectEl current optionValue optionLabel = do
  opt <- createElement "option" doc
  setAttribute "value" optionValue opt
  when (current == optionValue) (setAttribute "selected" "selected" opt)
  setTextContent optionLabel (toNode opt)
  _ <- appendChild (toNode opt) (toNode selectEl)
  pure unit

decodeCellsFromJson :: J.Json -> Array CellRow
decodeCellsFromJson json =
  case D.decodeJson json :: Either D.JsonDecodeError (Array J.Json) of
    Left _ -> []
    Right arr -> A.mapMaybe decodeCell arr

decodeCell :: J.Json -> Maybe CellRow
decodeCell json = do
  obj <- J.toObject json
  cellId <- readField ["id"] obj
  cellType <- readField ["cell_type", "cellType"] obj
  cellOrder <- readField ["cell_order", "cellOrder"] obj
  let content = readOptionalField ["content"] obj
  pure { id: cellId, cellType, cellOrder, content }

readField :: forall a. D.DecodeJson a => Array String -> FO.Object J.Json -> Maybe a
readField keys obj = do
  value <- firstPresent keys obj
  case D.decodeJson value of
    Left _ -> Nothing
    Right v -> Just v

readOptionalField :: forall a. D.DecodeJson a => Array String -> FO.Object J.Json -> Maybe a
readOptionalField keys obj =
  case firstPresent keys obj of
    Nothing -> Nothing
    Just value ->
      case D.decodeJson value of
        Left _ -> Nothing
        Right v -> v

firstPresent :: Array String -> FO.Object J.Json -> Maybe J.Json
firstPresent keys obj =
  case A.uncons keys of
    Nothing -> Nothing
    Just { head, tail } ->
      case FO.lookup head obj of
        Just v -> Just v
        Nothing -> firstPresent tail obj
