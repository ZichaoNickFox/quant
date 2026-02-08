{-# LANGUAGE OverloadedStrings #-}
module Tests.Integration.Web.Controller.CellControllerSpec (tests) where

import Config (config)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Data.List (findIndex)
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (callActionWithParams, responseBody, withContext)
import Prelude
import Test.Hspec
import Web.Controller.CellController ()
import Web.Controller.StrategyController ()
import Web.FrontController ()
import Web.Prelude
import Web.Types
import qualified Web.Types as Types

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "CellController (integration)" do
    it "CellCreateAction appends cell_order per owner" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000111"
      _ <- callActionWithParams CellCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerId))]
      _ <- callActionWithParams CellCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerId))]
      cells <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, ownerId) |> orderByAsc #cellOrder |> fetch
      map (get #cellOrder) cells `shouldBe` [1, 2]

    it "CellReadAction returns cells ordered by cell_order" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000112"
      _ <- newRecord @Cell
        |> set #cellType Raw
        |> set #ownerType CellOwnerTypeNote
        |> set #ownerId ownerId
        |> set #cellOrder 2
        |> set #content (Just "second")
        |> createRecord
      _ <- newRecord @Cell
        |> set #cellType Raw
        |> set #ownerType CellOwnerTypeNote
        |> set #ownerId ownerId
        |> set #cellOrder 1
        |> set #content (Just "first")
        |> createRecord
      response <- callActionWithParams CellReadAction [("ownerType", "note"), ("ownerId", cs (tshow ownerId))]
      body <- responseBody response
      decodeCellOrders body `shouldBe` Just [1, 2]

    it "CellReadAction returns [] when owner has no cells" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000123"
      before <- query @Cell
        |> filterWhere (#ownerType, CellOwnerTypeNote)
        |> filterWhere (#ownerId, ownerId)
        |> fetch
      length before `shouldBe` 0

      response <- callActionWithParams CellReadAction [("ownerType", "note"), ("ownerId", cs (tshow ownerId))]
      body <- responseBody response
      decodeCellOrders body `shouldBe` Just []

      after <- query @Cell
        |> filterWhere (#ownerType, CellOwnerTypeNote)
        |> filterWhere (#ownerId, ownerId)
        |> orderByAsc #cellOrder
        |> fetch
      map (get #cellOrder) after `shouldBe` []

    it "CellCreateAction keeps independent sequences per (owner_type, owner_id)" $ withContext do
      ownerA <- mkUuid "00000000-0000-0000-0000-000000000113"
      ownerB <- mkUuid "00000000-0000-0000-0000-000000000114"

      _ <- callActionWithParams CellCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerA))]
      _ <- callActionWithParams CellCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerA))]
      _ <- callActionWithParams CellCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerB))]
      _ <- callActionWithParams CellCreateAction [("ownerType", "strategy"), ("ownerId", cs (tshow ownerA))]

      noteA <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, ownerA) |> orderByAsc #cellOrder |> fetch
      noteB <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, ownerB) |> orderByAsc #cellOrder |> fetch
      strategyA <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeStrategy) |> filterWhere (#ownerId, ownerA) |> orderByAsc #cellOrder |> fetch
      map (get #cellOrder) noteA `shouldBe` [1, 2]
      map (get #cellOrder) noteB `shouldBe` [1]
      map (get #cellOrder) strategyA `shouldBe` [1]

    it "CellReadAction filters by owner and keeps order" $ withContext do
      targetOwner <- mkUuid "00000000-0000-0000-0000-000000000115"
      otherOwner <- mkUuid "00000000-0000-0000-0000-000000000116"
      _ <- newCell CellOwnerTypeNote targetOwner 2 (Just "second")
      _ <- newCell CellOwnerTypeNote targetOwner 1 (Just "first")
      _ <- newCell CellOwnerTypeNote otherOwner 1 (Just "other")
      _ <- newCell CellOwnerTypeStrategy targetOwner 1 (Just "strategy-owner")

      response <- callActionWithParams CellReadAction [("ownerType", "note"), ("ownerId", cs (tshow targetOwner))]
      body <- responseBody response
      let decoded = fromMaybe [] (decodeCellRows body)
      map decodedCellOrder decoded `shouldBe` [1, 2]
      map decodedCellContent decoded `shouldBe` [Just "first", Just "second"]
      all (\row -> decodedCellOwnerType row == "note" && decodedCellOwnerId row == tshow targetOwner) decoded `shouldBe` True

    it "CellUpdateAction updates type/content and supports clearing content" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000117"
      cell <- newCell CellOwnerTypeNote ownerId 1 (Just "old")
      let (Id cellId) = get #id cell

      _ <- callActionWithParams CellUpdateAction
        [ ("cellId", cs (tshow cellId))
        , ("cellType", "image")
        , ("content", "new")
        ]
      updated1 <- fetchOne (get #id cell)
      get #cellType updated1 `shouldBe` Image
      get #content updated1 `shouldBe` Just "new"

      _ <- callActionWithParams CellUpdateAction
        [ ("cellId", cs (tshow cellId))
        , ("cellType", "backtest")
        ]
      updated2 <- fetchOne (get #id cell)
      get #cellType updated2 `shouldBe` Backtest
      get #content updated2 `shouldBe` Nothing

    it "CellUpdateAction persists content and CellReadAction returns updated content" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000125"
      cell <- newCell CellOwnerTypeNote ownerId 1 (Just "old")
      let (Id cellId) = get #id cell
      _ <- callActionWithParams CellUpdateAction
        [ ("cellId", cs (tshow cellId))
        , ("cellType", "raw")
        , ("content", "persisted-content")
        ]
      response <- callActionWithParams CellReadAction [("ownerType", "note"), ("ownerId", cs (tshow ownerId))]
      body <- responseBody response
      let decoded = fromMaybe [] (decodeCellRows body)
      map decodedCellContent decoded `shouldBe` [Just "persisted-content"]

    it "CellDeleteAction deletes only target cell and preserves ordering progression" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000118"
      _ <- callActionWithParams CellCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerId))]
      _ <- callActionWithParams CellCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerId))]
      _ <- callActionWithParams CellCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerId))]
      cells0 <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, ownerId) |> orderByAsc #cellOrder |> fetch
      map (get #cellOrder) cells0 `shouldBe` [1, 2, 3]

      let middle = cells0 !! 1
      _ <- callActionWithParams CellDeleteAction [("cellId", cs (tshow (get #id middle)))]
      _ <- callActionWithParams CellCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerId))]

      cells1 <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, ownerId) |> orderByAsc #cellOrder |> fetch
      map (get #cellOrder) cells1 `shouldBe` [1, 3, 4]
      deleted <- query @Cell |> filterWhere (#id, get #id middle) |> fetchOneOrNothing
      deleted `shouldBe` Nothing

    it "CellCreateAtAction inserts above/below and shifts following orders" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000119"
      c1 <- newCell CellOwnerTypeNote ownerId 1 (Just "a")
      c2 <- newCell CellOwnerTypeNote ownerId 2 (Just "b")
      c3 <- newCell CellOwnerTypeNote ownerId 3 (Just "c")

      _ <- callActionWithParams CellCreateAtAction
        [ ("ownerType", "note")
        , ("ownerId", cs (tshow ownerId))
        , ("anchorCellId", cs (tshow (get #id c2)))
        , ("position", "above")
        ]
      _ <- callActionWithParams CellCreateAtAction
        [ ("ownerType", "note")
        , ("ownerId", cs (tshow ownerId))
        , ("anchorCellId", cs (tshow (get #id c3)))
        , ("position", "below")
        ]

      cells <- query @Cell
        |> filterWhere (#ownerType, CellOwnerTypeNote)
        |> filterWhere (#ownerId, ownerId)
        |> orderByAsc #cellOrder
        |> fetch
      map (get #cellOrder) cells `shouldBe` [1, 2, 3, 4, 5]
      length cells `shouldBe` 5
      -- original first remains first, original third was shifted by inserts
      let ids = map (get #id) cells
      all (`elem` ids) [get #id c1, get #id c2, get #id c3] `shouldBe` True
      let idx x = fromMaybe (-1) (findIndex (== x) ids)
      idx (get #id c2) `shouldBe` 2
      idx (get #id c3) `shouldBe` 3

    it "CellCreateAtAction falls back to append when anchor is invalid" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000120"
      _ <- newCell CellOwnerTypeNote ownerId 1 (Just "x")
      badAnchor <- mkUuid "00000000-0000-0000-0000-000000000121"
      _ <- callActionWithParams CellCreateAtAction
        [ ("ownerType", "note")
        , ("ownerId", cs (tshow ownerId))
        , ("anchorCellId", cs (tshow badAnchor))
        , ("position", "above")
        ]
      cells <- query @Cell
        |> filterWhere (#ownerType, CellOwnerTypeNote)
        |> filterWhere (#ownerId, ownerId)
        |> orderByAsc #cellOrder
        |> fetch
      map (get #cellOrder) cells `shouldBe` [1, 2]

    it "CellMoveAction swaps neighbors and keeps boundary moves as no-op" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000122"
      c1 <- newCell CellOwnerTypeNote ownerId 1 (Just "a")
      c2 <- newCell CellOwnerTypeNote ownerId 2 (Just "b")
      c3 <- newCell CellOwnerTypeNote ownerId 3 (Just "c")

      _ <- callActionWithParams CellMoveAction
        [ ("cellId", cs (tshow (get #id c2)))
        , ("direction", "up")
        ]
      afterUp <- query @Cell
        |> filterWhere (#ownerType, CellOwnerTypeNote)
        |> filterWhere (#ownerId, ownerId)
        |> orderByAsc #cellOrder
        |> fetch
      map (get #id) afterUp `shouldBe` [get #id c2, get #id c1, get #id c3]

      _ <- callActionWithParams CellMoveAction
        [ ("cellId", cs (tshow (get #id c2)))
        , ("direction", "up")
        ]
      afterBoundary <- query @Cell
        |> filterWhere (#ownerType, CellOwnerTypeNote)
        |> filterWhere (#ownerId, ownerId)
        |> orderByAsc #cellOrder
        |> fetch
      map (get #id) afterBoundary `shouldBe` [get #id c2, get #id c1, get #id c3]

      _ <- callActionWithParams CellMoveAction
        [ ("cellId", cs (tshow (get #id c1)))
        , ("direction", "down")
        ]
      afterDown <- query @Cell
        |> filterWhere (#ownerType, CellOwnerTypeNote)
        |> filterWhere (#ownerId, ownerId)
        |> orderByAsc #cellOrder
        |> fetch
      map (get #id) afterDown `shouldBe` [get #id c2, get #id c3, get #id c1]

    it "CellCreateAtAction does not clear existing content" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000124"
      c1 <- newCell CellOwnerTypeNote ownerId 1 (Just "alpha")
      c2 <- newCell CellOwnerTypeNote ownerId 2 (Just "beta")
      _ <- callActionWithParams CellCreateAtAction
        [ ("ownerType", "note")
        , ("ownerId", cs (tshow ownerId))
        , ("anchorCellId", cs (tshow (get #id c2)))
        , ("position", "above")
        ]
      c1' <- fetchOne (get #id c1)
      c2' <- fetchOne (get #id c2)
      get #content c1' `shouldBe` Just "alpha"
      get #content c2' `shouldBe` Just "beta"

    it "tree->cell flow uses strategy owner_id to read shared cells" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000130"
      -- Two different nodes under the same strategy owner
      _ <- newRecord @Tree
        |> set #ownerType TreeOwnerTypeStrategy
        |> set #ownerId ownerId
        |> set #nodeType Types.File
        |> set #parentTreeId Nothing
        |> set #nodeOrder 1
        |> createRecord
      _ <- newRecord @Tree
        |> set #ownerType TreeOwnerTypeStrategy
        |> set #ownerId ownerId
        |> set #nodeType Types.File
        |> set #parentTreeId Nothing
        |> set #nodeOrder 2
        |> createRecord

      _ <- newCell CellOwnerTypeStrategy ownerId 1 (Just "shared-strategy-cell")

      treeResponse <- callActionWithParams StrategyTreeReadAction [("strategyId", cs (tshow ownerId))]
      treeBody <- responseBody treeResponse
      let treeRows = fromMaybe [] (decodeTreeOwnerRows treeBody)
      length treeRows `shouldBe` 2
      all (== tshow ownerId) treeRows `shouldBe` True
      case treeRows of
        ownerFromTree : _ -> do
          cellResponse <- callActionWithParams CellReadAction [("ownerType", "strategy"), ("ownerId", cs ownerFromTree)]
          cellBody <- responseBody cellResponse
          let cells = fromMaybe [] (decodeCellRows cellBody)
          map decodedCellContent cells `shouldBe` [Just "shared-strategy-cell"]
        [] ->
          expectationFailure "StrategyTreeReadAction returned no rows"

mkUuid :: Text -> IO UUID
mkUuid t =
  pure (fromJust (UUID.fromText t))

decodeCellOrders :: LBS.ByteString -> Maybe [Int]
decodeCellOrders body = do
  value <- A.decode body
  arr <- case value of
    A.Array a -> Just (V.toList a)
    _ -> Nothing
  traverse decodeCellOrder arr

decodeCellOrder :: A.Value -> Maybe Int
decodeCellOrder (A.Object obj) = do
  v <- lookupObj "cell_order" obj <|> lookupObj "cellOrder" obj
  case v of
    A.Number n -> toBoundedInteger n
    _ -> Nothing
decodeCellOrder _ = Nothing

lookupObj :: Text -> A.Object -> Maybe A.Value
lookupObj key obj = AKM.lookup (AK.fromText key) obj

newCell :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> Int -> Maybe Text -> IO Cell
newCell ownerType ownerId order content =
  newRecord @Cell
    |> set #cellType Raw
    |> set #ownerType ownerType
    |> set #ownerId ownerId
    |> set #cellOrder order
    |> set #content content
    |> createRecord

data DecodedCell = DecodedCell
  { ownerType :: Text
  , ownerId :: Text
  , cellOrder :: Int
  , content :: Maybe Text
  }

decodeCellRows :: LBS.ByteString -> Maybe [DecodedCell]
decodeCellRows body = do
  value <- A.decode body
  arr <- case value of
    A.Array a -> Just (V.toList a)
    _ -> Nothing
  pure (mapMaybe decodeCellRow arr)

decodeCellRow :: A.Value -> Maybe DecodedCell
decodeCellRow (A.Object obj) = do
  ownerType <- lookupText ["owner_type", "ownerType"] obj
  ownerId <- lookupText ["owner_id", "ownerId"] obj
  cellOrder <- lookupInt ["cell_order", "cellOrder"] obj
  let content = lookupText ["content"] obj
  pure DecodedCell { ownerType, ownerId, cellOrder, content }
decodeCellRow _ = Nothing

lookupText :: [Text] -> A.Object -> Maybe Text
lookupText keys obj = do
  value <- lookupFirst keys obj
  case value of
    A.String t -> Just t
    _ -> Nothing

lookupInt :: [Text] -> A.Object -> Maybe Int
lookupInt keys obj = do
  value <- lookupFirst keys obj
  case value of
    A.Number n -> toBoundedInteger n
    _ -> Nothing

lookupFirst :: [Text] -> A.Object -> Maybe A.Value
lookupFirst [] _ = Nothing
lookupFirst (k:ks) obj =
  case AKM.lookup (AK.fromText k) obj of
    Just v -> Just v
    Nothing -> lookupFirst ks obj

decodedCellOwnerType :: DecodedCell -> Text
decodedCellOwnerType DecodedCell { ownerType = x } = x

decodedCellOwnerId :: DecodedCell -> Text
decodedCellOwnerId DecodedCell { ownerId = x } = x

decodedCellOrder :: DecodedCell -> Int
decodedCellOrder DecodedCell { cellOrder = x } = x

decodedCellContent :: DecodedCell -> Maybe Text
decodedCellContent DecodedCell { content = x } = x

decodeTreeOwnerRows :: LBS.ByteString -> Maybe [Text]
decodeTreeOwnerRows body = do
  value <- A.decode body
  arr <- case value of
    A.Array a -> Just (V.toList a)
    _ -> Nothing
  traverse decodeTreeOwnerId arr

decodeTreeOwnerId :: A.Value -> Maybe Text
decodeTreeOwnerId (A.Object obj) = lookupText ["owner_id", "ownerId"] obj
decodeTreeOwnerId _ = Nothing
