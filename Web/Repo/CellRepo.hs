module Web.Repo.CellRepo
  ( createCell
  , createCellAbove
  , createCellBelow
  , ensureFirstCell
  , loadCells
  , updateCell
  , moveCellUp
  , moveCellDown
  , deleteCellById
  ) where

import Database.PostgreSQL.Simple (Only (..))
import Data.Maybe (listToMaybe)
import Web.Prelude
import Web.Types

createCell :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> IO Cell
createCell ownerType ownerId = do
  lockCellOrder ownerType ownerId
  order <- nextCellOrder ownerType ownerId
  createAtOrder ownerType ownerId order

createCellAbove :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> Id Cell -> IO Cell
createCellAbove ownerType ownerId anchorCellId = do
  createCellAt ownerType ownerId anchorCellId False

createCellBelow :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> Id Cell -> IO Cell
createCellBelow ownerType ownerId anchorCellId = do
  createCellAt ownerType ownerId anchorCellId True

createCellAt :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> Id Cell -> Bool -> IO Cell
createCellAt ownerType ownerId anchorCellId insertBelow = do
  lockCellOrder ownerType ownerId
  mbAnchor <- query @Cell
    |> filterWhere (#id, anchorCellId)
    |> filterWhere (#ownerType, ownerType)
    |> filterWhere (#ownerId, ownerId)
    |> fetchOneOrNothing
  case mbAnchor of
    Nothing -> createCell ownerType ownerId
    Just anchor -> do
      let anchorOrder = get #cellOrder anchor
          insertOrder = if insertBelow then anchorOrder + 1 else anchorOrder
      shiftOrdersForInsert ownerType ownerId insertOrder
      createAtOrder ownerType ownerId insertOrder

shiftOrdersForInsert :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> Int -> IO ()
shiftOrdersForInsert ownerType ownerId insertOrder = do
  let tempOffset = (1000000 :: Int)
  _ <- sqlExec
    "UPDATE cell SET cell_order = cell_order + ? WHERE owner_type = ? AND owner_id = ? AND cell_order >= ?"
    (tempOffset, ownerType, ownerId, insertOrder)
  _ <- sqlExec
    "UPDATE cell SET cell_order = cell_order - ? WHERE owner_type = ? AND owner_id = ? AND cell_order >= ?"
    (tempOffset - 1, ownerType, ownerId, insertOrder + tempOffset)
  pure ()

createAtOrder :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> Int -> IO Cell
createAtOrder ownerType ownerId order =
  newRecord @Cell
    |> set #cellType Raw
    |> set #ownerType ownerType
    |> set #ownerId ownerId
    |> set #cellOrder order
    |> set #content (Just "")
    |> createRecord

ensureFirstCell :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> IO ()
ensureFirstCell ownerType ownerId = do
  lockCellOrder ownerType ownerId
  mb <- query @Cell
    |> filterWhere (#ownerType, ownerType)
    |> filterWhere (#ownerId, ownerId)
    |> fetchOneOrNothing
  case mb of
    Just _ -> pure ()
    Nothing -> do
      _ <- createAtOrder ownerType ownerId 1
      pure ()

loadCells :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> IO [Cell]
loadCells ownerType ownerId =
  query @Cell
    |> filterWhere (#ownerType, ownerType)
    |> filterWhere (#ownerId, ownerId)
    |> orderByAsc #cellOrder
    |> fetch

updateCell :: (?modelContext :: ModelContext) => Id Cell -> CellType -> Maybe Text -> IO ()
updateCell cellId cellType content = do
  cell <- fetchOne cellId
  cell
    |> set #cellType cellType
    |> set #content content
    |> updateRecord
  pure ()

moveCellUp :: (?modelContext :: ModelContext) => Id Cell -> IO ()
moveCellUp cellId = moveCell cellId False

moveCellDown :: (?modelContext :: ModelContext) => Id Cell -> IO ()
moveCellDown cellId = moveCell cellId True

moveCell :: (?modelContext :: ModelContext) => Id Cell -> Bool -> IO ()
moveCell cellId isDown = do
  cell <- fetchOne cellId
  let ownerType = get #ownerType cell
      ownerId = get #ownerId cell
      currentOrder = get #cellOrder cell
  lockCellOrder ownerType ownerId
  cells <- loadCells ownerType ownerId
  let mbTarget = neighboringCell isDown cellId cells
  case mbTarget of
    Nothing -> pure ()
    Just target -> do
      let targetOrder = get #cellOrder target
          tempOrder = maybe 0 ((subtract 1) . get #cellOrder) (listToMaybe cells)
      _ <- sqlExec "UPDATE cell SET cell_order = ? WHERE id = ?" (tempOrder, cellId)
      _ <- sqlExec "UPDATE cell SET cell_order = ? WHERE id = ?" (currentOrder, get #id target)
      _ <- sqlExec "UPDATE cell SET cell_order = ? WHERE id = ?" (targetOrder, cellId)
      pure ()

neighboringCell :: Bool -> Id Cell -> [Cell] -> Maybe Cell
neighboringCell isDown targetId cells = go Nothing cells
  where
    go _ [] = Nothing
    go prev (x:xs)
      | get #id x == targetId =
          if isDown then listToMaybe xs else prev
      | otherwise = go (Just x) xs

deleteCellById :: (?modelContext :: ModelContext) => Id Cell -> IO ()
deleteCellById cellId = do
  cell <- fetchOne cellId
  deleteRecord cell

lockCellOrder :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> IO ()
lockCellOrder ownerType ownerId = do
  let lockKey = inputValue ownerType <> ":" <> tshow ownerId
  -- Serialize creates per (ownerType, ownerId) within the current transaction.
  _ <- (sqlQuery "SELECT pg_advisory_xact_lock(hashtext(?))" (Only lockKey) :: IO [Only ()])
  pure ()

nextCellOrder :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> IO Int
nextCellOrder ownerType ownerId = do
  cells <- loadCells ownerType ownerId
  let mb = lastMay cells
  pure $ maybe 1 ((+ 1) . get #cellOrder) mb
