module Common.CellWidget
  ( buildCellUpdateInput
  , CellRow
  , cellTypeOptions
  , CellUpdateInput
  , createFRP
  , decodeCellsFromJson
  , Events
  , isChartCellType
  ) where

import Data.Argonaut.Core as J

import Data.Argonaut.Decode as D
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import FFI.LightweightCharts as LightweightCharts
import Foreign.Object as FO
import FRP as FRP
import FRP.Component.TextArea as Textarea
import Prelude
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element (Element, setAttribute, toEventTarget, toNode)
import Web.DOM.Node (appendChild, setTextContent)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.HTMLInputElement as HTMLInput
import Web.HTML.HTMLSelectElement as HTMLSelect
import Web.HTML.HTMLTextAreaElement as HTMLTextArea

type Events =
  { element :: Element
  , init :: Effect Unit
  , onSaveSubs :: FRP.Event CellUpdateInput
  , onDeleteSubs :: FRP.Event String
  , onInsertAboveSubs :: FRP.Event { ownerId :: String, cellId :: String }
  , onInsertBelowSubs :: FRP.Event { ownerId :: String, cellId :: String }
  , onMoveUpSubs :: FRP.Event String
  , onMoveDownSubs :: FRP.Event String
  }

createFRP :: Document -> String -> Effect String -> CellRow -> Effect Events
createFRP doc ownerType readOwnerId cell = do
  { event: onSaveSubs, push: pushSave } <- FRP.create
  { event: onDeleteSubs, push: pushDelete } <- FRP.create
  { event: onInsertAboveSubs, push: pushInsertAbove } <- FRP.create
  { event: onInsertBelowSubs, push: pushInsertBelow } <- FRP.create
  { event: onMoveUpSubs, push: pushMoveUp } <- FRP.create
  { event: onMoveDownSubs, push: pushMoveDown } <- FRP.create

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

  headerRow <- createElement "div" doc
  setAttribute "class" "d-flex gap-2 flex-wrap align-items-center mb-2" headerRow

  typeSelect <- createElement "select" doc
  setAttribute "name" "cellType" typeSelect
  setAttribute "class" "form-select form-select-sm w-auto" typeSelect
  setAttribute "style" "height: 32px;" typeSelect
  for_ cellTypeOptions \(Tuple value label) ->
    appendCellTypeOption doc typeSelect cell.cellType value label

  let contentText = fromMaybe "" cell.content
  textAreaHandle <- Textarea.createFRP doc
    { name: "content"
    , className: "form-control mb-2"
    , rows: 1
    , value: contentText
    , events: Nothing
    }
  let contentEl = textAreaHandle.element

  let mCellIdInput = HTMLInput.fromElement idInput
  let mCellTypeSelect = HTMLSelect.fromElement typeSelect
  let mContentInput = HTMLTextArea.fromElement contentEl

  _ <- appendChild (toNode idInput) (toNode updateCard)
  _ <- appendChild (toNode headerRow) (toNode updateCard)
  appendLightweightChartIfNeeded doc updateCard cell
  _ <- appendChild (toNode contentEl) (toNode updateCard)

  delBtn <- createElement "button" doc
  setAttribute "type" "button" delBtn
  setAttribute "class" "btn btn-sm btn-outline-danger" delBtn
  setAttribute "style" "height: 32px;" delBtn
  setAttribute "data-cell-delete-button" "1" delBtn
  setTextContent "Delete" (toNode delBtn)

  actionRow <- createElement "div" doc
  setAttribute "class" "d-flex gap-2 flex-wrap" actionRow

  addAboveBtn <- createElement "button" doc
  setAttribute "type" "button" addAboveBtn
  setAttribute "class" "btn btn-sm btn-outline-secondary" addAboveBtn
  setAttribute "style" "height: 32px;" addAboveBtn
  setAttribute "data-cell-add-above" "1" addAboveBtn
  setTextContent "Insert Above" (toNode addAboveBtn)

  addBelowBtn <- createElement "button" doc
  setAttribute "type" "button" addBelowBtn
  setAttribute "class" "btn btn-sm btn-outline-secondary" addBelowBtn
  setAttribute "style" "height: 32px;" addBelowBtn
  setAttribute "data-cell-add-below" "1" addBelowBtn
  setTextContent "Insert Below" (toNode addBelowBtn)

  moveUpBtn <- createElement "button" doc
  setAttribute "type" "button" moveUpBtn
  setAttribute "class" "btn btn-sm btn-outline-secondary" moveUpBtn
  setAttribute "style" "height: 32px;" moveUpBtn
  setAttribute "data-cell-move-up" "1" moveUpBtn
  setTextContent "Move Up" (toNode moveUpBtn)

  moveDownBtn <- createElement "button" doc
  setAttribute "type" "button" moveDownBtn
  setAttribute "class" "btn btn-sm btn-outline-secondary" moveDownBtn
  setAttribute "style" "height: 32px;" moveDownBtn
  setAttribute "data-cell-move-down" "1" moveDownBtn
  setTextContent "Move Down" (toNode moveDownBtn)

  _ <- appendChild (toNode delBtn) (toNode actionRow)
  _ <- appendChild (toNode addAboveBtn) (toNode actionRow)
  _ <- appendChild (toNode addBelowBtn) (toNode actionRow)
  _ <- appendChild (toNode moveUpBtn) (toNode actionRow)
  _ <- appendChild (toNode moveDownBtn) (toNode actionRow)

  _ <- appendChild (toNode typeSelect) (toNode headerRow)
  _ <- appendChild (toNode actionRow) (toNode headerRow)
  _ <- appendChild (toNode updateCard) (toNode body)
  _ <- appendChild (toNode body) (toNode card)

  let readCellId = case mCellIdInput of
        Nothing -> pure ""
        Just cellIdInput -> HTMLInput.value cellIdInput

  for_ mContentInput \contentInput -> do
    onBlur <- eventListener \_ -> do
      mbInput <- readUpdateInput mCellIdInput mCellTypeSelect (Just contentInput)
      for_ mbInput pushSave
    addEventListener (EventType "blur") onBlur false (toEventTarget contentEl)

  onDelete <- eventListener \_ -> do
    cellId <- readCellId
    when (cellId /= "") (pushDelete cellId)
  addEventListener (EventType "click") onDelete false (toEventTarget delBtn)

  onAddAbove <- eventListener \_ -> do
    cellId <- readCellId
    ownerId <- readOwnerId
    when (cellId /= "" && ownerId /= "") (pushInsertAbove { ownerId, cellId })
  addEventListener (EventType "click") onAddAbove false (toEventTarget addAboveBtn)

  onAddBelow <- eventListener \_ -> do
    cellId <- readCellId
    ownerId <- readOwnerId
    when (cellId /= "" && ownerId /= "") (pushInsertBelow { ownerId, cellId })
  addEventListener (EventType "click") onAddBelow false (toEventTarget addBelowBtn)

  onMoveUp <- eventListener \_ -> do
    cellId <- readCellId
    when (cellId /= "") (pushMoveUp cellId)
  addEventListener (EventType "click") onMoveUp false (toEventTarget moveUpBtn)

  onMoveDown <- eventListener \_ -> do
    cellId <- readCellId
    when (cellId /= "") (pushMoveDown cellId)
  addEventListener (EventType "click") onMoveDown false (toEventTarget moveDownBtn)

  pure
    { element: card
    , init: textAreaHandle.init
    , onSaveSubs
    , onDeleteSubs
    , onInsertAboveSubs
    , onInsertBelowSubs
    , onMoveUpSubs
    , onMoveDownSubs
    }

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

cellTypeOptions :: Array (Tuple String String)
cellTypeOptions =
  [ Tuple "raw" "Raw"
  , Tuple "image" "Image"
  , Tuple "backtest" "Backtest"
  , Tuple "chart" "Chart"
  ]

isChartCellType :: String -> Boolean
isChartCellType cellType =
  cellType == "chart" || cellType == "lightweight_charts"

buildCellUpdateInput :: String -> String -> String -> Maybe CellUpdateInput
buildCellUpdateInput cellId cellType content =
  if cellId == ""
    then Nothing
    else Just { cellId, cellType, content }


readUpdateInput
  :: Maybe HTMLInput.HTMLInputElement
  -> Maybe HTMLSelect.HTMLSelectElement
  -> Maybe HTMLTextArea.HTMLTextAreaElement
  -> Effect (Maybe CellUpdateInput)
readUpdateInput mCellIdInput mCellTypeSelect mContentInput =
  case Tuple mCellIdInput mCellTypeSelect of
    Tuple (Just cellIdInput) (Just cellTypeSelect) -> do
      cellId <- HTMLInput.value cellIdInput
      cellType <- HTMLSelect.value cellTypeSelect
      content <- case mContentInput of
        Just contentEl -> HTMLTextArea.value contentEl
        Nothing -> pure ""
      pure $ buildCellUpdateInput cellId cellType content
    _ -> pure Nothing

appendCellTypeOption :: Document -> Element -> String -> String -> String -> Effect Unit
appendCellTypeOption doc selectEl current optionValue optionLabel = do
  opt <- createElement "option" doc
  setAttribute "value" optionValue opt
  when (current == optionValue) (setAttribute "selected" "selected" opt)
  setTextContent optionLabel (toNode opt)
  _ <- appendChild (toNode opt) (toNode selectEl)
  pure unit

appendLightweightChartIfNeeded :: Document -> Element -> CellRow -> Effect Unit
appendLightweightChartIfNeeded doc updateCard cell = do
  when (isChartCellType cell.cellType) do
    chartEl <- createElement "div" doc
    setAttribute "class" "border rounded mb-2" chartEl
    setAttribute "style" "height: 320px;" chartEl
    _ <- appendChild (toNode chartEl) (toNode updateCard)
    chart <- LightweightCharts.createChart chartEl
    series <- LightweightCharts.addCandlestickSeries chart
    for_ cell.content \contentJson ->
      when (contentJson /= "") do
        LightweightCharts.setDataFromJson series contentJson
        LightweightCharts.fitContent chart

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
