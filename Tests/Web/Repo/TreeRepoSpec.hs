{-# LANGUAGE OverloadedStrings #-}
module Tests.Web.Repo.TreeRepoSpec (tests) where

import Config (config)
import qualified Data.UUID as UUID
import Data.Maybe (fromJust)
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (withContext)
import Prelude
import Test.Hspec
import Web.FrontController ()
import Web.Prelude
import Web.Repo.TreeRepo as TreeRepo
import Web.Types
import qualified Web.Types as Types

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "TreeRepo (integration)" do
    it "createTreeNode assigns node_order by sibling group and isolates owner scope" $ withContext do
      ownerA <- mkUuid "00000000-0000-0000-0000-000000001121"
      ownerB <- mkUuid "00000000-0000-0000-0000-000000001122"
      rootA <- TreeRepo.createTreeNode TreeOwnerTypeNote ownerA Types.File "root-a" Nothing
      rootB <- TreeRepo.createTreeNode TreeOwnerTypeNote ownerA Types.File "root-b" Nothing
      let (Id rootAUuid) = get #id rootA
          (Id rootBUuid) = get #id rootB
      _ <- TreeRepo.createTreeNode TreeOwnerTypeNote ownerA Types.File "child-a-1" (Just rootAUuid)
      _ <- TreeRepo.createTreeNode TreeOwnerTypeNote ownerA Types.File "child-a-2" (Just rootAUuid)
      _ <- TreeRepo.createTreeNode TreeOwnerTypeNote ownerA Types.File "child-b-1" (Just rootBUuid)
      _ <- TreeRepo.createTreeNode TreeOwnerTypeNote ownerB Types.File "other-owner-root" Nothing
      _ <- TreeRepo.createTreeNode TreeOwnerTypeStrategy ownerA Types.File "strategy-root" Nothing

      rootsA <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeNote)
        |> filterWhere (#ownerId, ownerA)
        |> filterWhere (#parentTreeId, Nothing)
        |> orderByAsc #nodeOrder
        |> fetch
      map (get #nodeOrder) rootsA `shouldBe` [1, 2]

      childrenA <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeNote)
        |> filterWhere (#ownerId, ownerA)
        |> filterWhere (#parentTreeId, Just rootAUuid)
        |> orderByAsc #nodeOrder
        |> fetch
      map (get #nodeOrder) childrenA `shouldBe` [1, 2]

    it "loadTree filters by owner and returns nodes ordered by node_order" $ withContext do
      ownerA <- mkUuid "00000000-0000-0000-0000-000000001123"
      ownerB <- mkUuid "00000000-0000-0000-0000-000000001124"
      _ <- newTree ownerA "n-3" Nothing 3
      _ <- newTree ownerA "n-1" Nothing 1
      _ <- newTree ownerA "n-2" Nothing 2
      _ <- newTree ownerB "other-owner" Nothing 1
      _ <- newTreeStrategy ownerA "other-type" Nothing 1

      rows <- TreeRepo.loadTree TreeOwnerTypeNote ownerA
      map (get #name) rows `shouldBe` ["n-1", "n-2", "n-3"]

    it "updateTreeNode applies fields and rejects invalid parent assignments" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000001125"
      root <- TreeRepo.createTreeNode TreeOwnerTypeNote ownerId Types.File "root" Nothing
      let (Id rootId) = get #id root
      child <- TreeRepo.createTreeNode TreeOwnerTypeNote ownerId Types.File "child" (Just rootId)
      let (Id childId) = get #id child

      -- self parent should be rejected
      TreeRepo.updateTreeNode (get #id root) Nothing (Just rootId) 1
      updatedRoot <- fetchOne (get #id root)
      get #parentTreeId updatedRoot `shouldBe` Nothing

      -- cycle parent should be rejected while child is still under root
      TreeRepo.updateTreeNode (get #id root) Nothing (Just childId) 1
      updatedRoot2 <- fetchOne (get #id root)
      get #parentTreeId updatedRoot2 `shouldBe` Nothing

      -- normal update should still work
      TreeRepo.updateTreeNode (get #id child) (Just "child-renamed") Nothing 2
      updated1 <- fetchOne (get #id child)
      get #name updated1 `shouldBe` "child-renamed"
      get #parentTreeId updated1 `shouldBe` Nothing
      get #nodeOrder updated1 `shouldBe` 2

    it "deleteTreeNode removes subtree and note-owned cells under subtree ids" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000001126"
      root <- newTree ownerId "root-del" Nothing 1
      let (Id rootId) = get #id root
      child <- newTree ownerId "child-del" (Just rootId) 1
      let (Id childId) = get #id child
      keep <- newTree ownerId "keep" Nothing 2
      let (Id keepId) = get #id keep

      _ <- newCell CellOwnerTypeNote rootId 1 "root-cell"
      _ <- newCell CellOwnerTypeNote childId 1 "child-cell"
      _ <- newCell CellOwnerTypeNote keepId 1 "keep-cell"
      _ <- newCell CellOwnerTypeStrategy rootId 1 "strategy-cell-should-keep"

      TreeRepo.deleteTreeNode (get #id root)

      deletedRoot <- query @Tree |> filterWhere (#id, get #id root) |> fetchOneOrNothing
      deletedChild <- query @Tree |> filterWhere (#id, get #id child) |> fetchOneOrNothing
      existingKeep <- query @Tree |> filterWhere (#id, get #id keep) |> fetchOneOrNothing
      deletedRoot `shouldBe` Nothing
      deletedChild `shouldBe` Nothing
      existingKeep `shouldNotBe` Nothing

      rootCells <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, rootId) |> fetch
      childCells <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, childId) |> fetch
      keepCells <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, keepId) |> fetch
      strategyCells <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeStrategy) |> filterWhere (#ownerId, rootId) |> fetch
      length rootCells `shouldBe` 0
      length childCells `shouldBe` 0
      length keepCells `shouldBe` 1
      length strategyCells `shouldBe` 1

mkUuid :: Text -> IO UUID
mkUuid t =
  pure (fromJust (UUID.fromText t))

newTree :: (?modelContext :: ModelContext) => UUID -> Text -> Maybe UUID -> Int -> IO Tree
newTree ownerId name parentTreeId nodeOrder =
  newRecord @Tree
    |> set #ownerType TreeOwnerTypeNote
    |> set #ownerId ownerId
    |> set #nodeType Types.File
    |> set #name name
    |> set #parentTreeId parentTreeId
    |> set #nodeOrder nodeOrder
    |> createRecord

newTreeStrategy :: (?modelContext :: ModelContext) => UUID -> Text -> Maybe UUID -> Int -> IO Tree
newTreeStrategy ownerId name parentTreeId nodeOrder =
  newRecord @Tree
    |> set #ownerType TreeOwnerTypeStrategy
    |> set #ownerId ownerId
    |> set #nodeType Types.File
    |> set #name name
    |> set #parentTreeId parentTreeId
    |> set #nodeOrder nodeOrder
    |> createRecord

newCell :: (?modelContext :: ModelContext) => CellOwnerType -> UUID -> Int -> Text -> IO Cell
newCell ownerType ownerId order content =
  newRecord @Cell
    |> set #cellType Raw
    |> set #ownerType ownerType
    |> set #ownerId ownerId
    |> set #cellOrder order
    |> set #content (Just content)
    |> createRecord
