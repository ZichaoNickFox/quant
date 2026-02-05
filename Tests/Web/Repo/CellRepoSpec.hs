{-# LANGUAGE OverloadedStrings #-}
module Tests.Web.Repo.CellRepoSpec (tests) where

import Config (config)
import qualified Data.UUID as UUID
import Data.Maybe (fromJust)
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (withContext)
import Prelude
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Test.Hspec
import Web.FrontController ()
import Web.Prelude
import Web.Repo.CellRepo as CellRepo
import Web.Types

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "CellRepo (integration)" do
    it "createCell appends cell_order per owner" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000001111"
      _ <- CellRepo.createCell CellOwnerTypeNote ownerId
      _ <- CellRepo.createCell CellOwnerTypeNote ownerId
      cells <- CellRepo.loadCells CellOwnerTypeNote ownerId
      map (get #cellOrder) cells `shouldBe` [1, 2]

    it "createCell keeps independent sequences per (owner_type, owner_id)" $ withContext do
      ownerA <- mkUuid "00000000-0000-0000-0000-000000001112"
      ownerB <- mkUuid "00000000-0000-0000-0000-000000001113"
      _ <- CellRepo.createCell CellOwnerTypeNote ownerA
      _ <- CellRepo.createCell CellOwnerTypeNote ownerA
      _ <- CellRepo.createCell CellOwnerTypeNote ownerB
      _ <- CellRepo.createCell CellOwnerTypeStrategy ownerA

      noteA <- CellRepo.loadCells CellOwnerTypeNote ownerA
      noteB <- CellRepo.loadCells CellOwnerTypeNote ownerB
      strategyA <- CellRepo.loadCells CellOwnerTypeStrategy ownerA
      map (get #cellOrder) noteA `shouldBe` [1, 2]
      map (get #cellOrder) noteB `shouldBe` [1]
      map (get #cellOrder) strategyA `shouldBe` [1]

    it "loadCells returns rows ordered by cell_order" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000001114"
      _ <- newCell CellOwnerTypeNote ownerId 2 (Just "second")
      _ <- newCell CellOwnerTypeNote ownerId 1 (Just "first")
      cells <- CellRepo.loadCells CellOwnerTypeNote ownerId
      map (get #cellOrder) cells `shouldBe` [1, 2]
      map (get #content) cells `shouldBe` [Just "first", Just "second"]

    it "updateCell updates both type and content" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000001115"
      cell <- newCell CellOwnerTypeNote ownerId 1 (Just "old")
      CellRepo.updateCell (get #id cell) Image (Just "new")
      updated <- fetchOne (get #id cell)
      get #cellType updated `shouldBe` Image
      get #content updated `shouldBe` Just "new"

    it "deleteCellById removes target row only" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000001116"
      c1 <- newCell CellOwnerTypeNote ownerId 1 (Just "a")
      c2 <- newCell CellOwnerTypeNote ownerId 2 (Just "b")
      CellRepo.deleteCellById (get #id c1)
      deleted <- query @Cell |> filterWhere (#id, get #id c1) |> fetchOneOrNothing
      remained <- query @Cell |> filterWhere (#id, get #id c2) |> fetchOneOrNothing
      deleted `shouldBe` Nothing
      remained `shouldNotBe` Nothing

    it "createCellAbove and createCellBelow insert at correct positions" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000001117"
      c1 <- newCell CellOwnerTypeNote ownerId 1 (Just "a")
      c2 <- newCell CellOwnerTypeNote ownerId 2 (Just "b")
      c3 <- newCell CellOwnerTypeNote ownerId 3 (Just "c")

      _ <- CellRepo.createCellAbove CellOwnerTypeNote ownerId (get #id c2)
      _ <- CellRepo.createCellBelow CellOwnerTypeNote ownerId (get #id c3)

      cells <- CellRepo.loadCells CellOwnerTypeNote ownerId
      map (get #cellOrder) cells `shouldBe` [1, 2, 3, 4, 5]
      let ids = map (get #id) cells
      all (`elem` ids) [get #id c1, get #id c2, get #id c3] `shouldBe` True
      let idx x = fromMaybe (-1) (findIndex (== x) ids)
      idx (get #id c2) `shouldBe` 2  -- shifted to after newly inserted above row
      idx (get #id c3) `shouldBe` 3  -- shifted to before newly inserted below row

    it "createCellAbove falls back to append when anchor does not belong to owner" $ withContext do
      ownerA <- mkUuid "00000000-0000-0000-0000-000000001118"
      ownerB <- mkUuid "00000000-0000-0000-0000-000000001119"
      _ <- newCell CellOwnerTypeNote ownerA 1 (Just "a")
      anchorB <- newCell CellOwnerTypeNote ownerB 1 (Just "b")
      _ <- CellRepo.createCellAbove CellOwnerTypeNote ownerA (get #id anchorB)
      cellsA <- CellRepo.loadCells CellOwnerTypeNote ownerA
      map (get #cellOrder) cellsA `shouldBe` [1, 2]

    it "moveCellUp and moveCellDown swap neighbors; boundaries are no-op" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000001120"
      c1 <- newCell CellOwnerTypeNote ownerId 1 (Just "a")
      c2 <- newCell CellOwnerTypeNote ownerId 2 (Just "b")
      c3 <- newCell CellOwnerTypeNote ownerId 3 (Just "c")

      CellRepo.moveCellUp (get #id c2)
      afterUp <- CellRepo.loadCells CellOwnerTypeNote ownerId
      map (get #id) afterUp `shouldBe` [get #id c2, get #id c1, get #id c3]

      CellRepo.moveCellUp (get #id c2)
      afterUpBoundary <- CellRepo.loadCells CellOwnerTypeNote ownerId
      map (get #id) afterUpBoundary `shouldBe` [get #id c2, get #id c1, get #id c3]

      CellRepo.moveCellDown (get #id c1)
      afterDown <- CellRepo.loadCells CellOwnerTypeNote ownerId
      map (get #id) afterDown `shouldBe` [get #id c2, get #id c3, get #id c1]

    it "ensureFirstCell creates one cell for empty owner and is idempotent" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000001121"
      initial <- CellRepo.loadCells CellOwnerTypeNote ownerId
      length initial `shouldBe` 0

      CellRepo.ensureFirstCell CellOwnerTypeNote ownerId
      after1 <- CellRepo.loadCells CellOwnerTypeNote ownerId
      map (get #cellOrder) after1 `shouldBe` [1]

      CellRepo.ensureFirstCell CellOwnerTypeNote ownerId
      after2 <- CellRepo.loadCells CellOwnerTypeNote ownerId
      map (get #cellOrder) after2 `shouldBe` [1]
      length after2 `shouldBe` 1

    it "createCellAbove keeps existing cell content unchanged" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000001122"
      c1 <- newCell CellOwnerTypeNote ownerId 1 (Just "alpha")
      c2 <- newCell CellOwnerTypeNote ownerId 2 (Just "beta")
      _ <- CellRepo.createCellAbove CellOwnerTypeNote ownerId (get #id c2)
      c1' <- fetchOne (get #id c1)
      c2' <- fetchOne (get #id c2)
      get #content c1' `shouldBe` Just "alpha"
      get #content c2' `shouldBe` Just "beta"

mkUuid :: Text -> IO UUID
mkUuid t =
  pure (fromJust (UUID.fromText t))

newCell :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> Int -> Maybe Text -> IO Cell
newCell ownerType ownerId order content =
  newRecord @Cell
    |> set #cellType Raw
    |> set #ownerType ownerType
    |> set #ownerId ownerId
    |> set #cellOrder order
    |> set #content content
    |> createRecord
