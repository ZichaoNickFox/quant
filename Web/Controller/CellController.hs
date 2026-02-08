module Web.Controller.CellController where

import qualified Data.Aeson as A
import Web.Prelude
import Web.Repo.CellRepo as CellRepo
import Web.Types

instance Controller CellController where
  action CellCreateAction = do
    let ownerType = param @CellOwnerType "ownerType"
        ownerId = param @UUID "ownerId"
    cell <- CellRepo.createCell ownerType ownerId
    renderJson (cellToJson cell)

  action CellCreateAtAction = do
    let ownerType = param @CellOwnerType "ownerType"
        ownerId = param @UUID "ownerId"
        anchorCellId = Id (param @UUID "anchorCellId") :: Id Cell
        position = param @Text "position"
    cell <- if position == "above"
      then CellRepo.createCellAbove ownerType ownerId anchorCellId
      else CellRepo.createCellBelow ownerType ownerId anchorCellId
    renderJson (cellToJson cell)

  action CellReadAction = do
    let ownerType = param @CellOwnerType "ownerType"
        ownerId = param @UUID "ownerId"
    cells <- CellRepo.loadCells ownerType ownerId
    renderJson (map cellToJson cells)

  action CellUpdateAction = do
    let cellId = Id (param @UUID "cellId") :: Id Cell
        cellType = param @CellType "cellType"
        content = paramOrNothing @Text "content"
    CellRepo.updateCell cellId cellType content
    renderJson (A.object [ "ok" A..= True ])

  action CellDeleteAction = do
    let cellId = Id (param @UUID "cellId") :: Id Cell
    CellRepo.deleteCellById cellId
    renderJson (A.object [ "ok" A..= True ])

  action CellMoveAction = do
    let cellId = Id (param @UUID "cellId") :: Id Cell
        direction = param @Text "direction"
    if direction == "up"
      then CellRepo.moveCellUp cellId
      else CellRepo.moveCellDown cellId
    renderJson (A.object [ "ok" A..= True ])

cellToJson :: Cell -> A.Value
cellToJson cell =
  A.object
    [ "id" A..= get #id cell
    , "cell_type" A..= inputValue (get #cellType cell)
    , "owner_type" A..= inputValue (get #ownerType cell)
    , "owner_id" A..= get #ownerId cell
    , "cell_order" A..= get #cellOrder cell
    , "content" A..= get #content cell
    ]
