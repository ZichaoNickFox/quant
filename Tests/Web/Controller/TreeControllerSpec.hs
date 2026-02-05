{-# LANGUAGE OverloadedStrings #-}
module Tests.Web.Controller.TreeControllerSpec (tests) where

import Config (config)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (callActionWithParams, responseBody, withContext)
import Prelude
import Test.Hspec
import Web.Controller.TreeController ()
import Web.FrontController ()
import Web.Prelude
import Web.Types
import qualified Web.Types as Types

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "TreeController (integration)" do
    it "TreeCreateAction assigns node_order by sibling group and isolates owner scope" $ withContext do
      ownerA <- mkUuid "00000000-0000-0000-0000-000000000121"
      ownerB <- mkUuid "00000000-0000-0000-0000-000000000122"
      _ <- callActionWithParams TreeCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerA)), ("nodeType", "file"), ("name", "root-a")]
      _ <- callActionWithParams TreeCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerA)), ("nodeType", "file"), ("name", "root-b")]
      rootA <- query @Tree |> filterWhere (#ownerType, TreeOwnerTypeNote) |> filterWhere (#ownerId, ownerA) |> filterWhere (#name, "root-a") |> fetchOneOrNothing >>= expectJust "root-a missing"
      rootB <- query @Tree |> filterWhere (#ownerType, TreeOwnerTypeNote) |> filterWhere (#ownerId, ownerA) |> filterWhere (#name, "root-b") |> fetchOneOrNothing >>= expectJust "root-b missing"
      let (Id rootAUuid) = get #id rootA
          (Id rootBUuid) = get #id rootB
      _ <- callActionWithParams TreeCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerA)), ("nodeType", "file"), ("name", "child-a-1"), ("parentTreeId", cs (tshow rootAUuid))]
      _ <- callActionWithParams TreeCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerA)), ("nodeType", "file"), ("name", "child-a-2"), ("parentTreeId", cs (tshow rootAUuid))]
      _ <- callActionWithParams TreeCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerA)), ("nodeType", "file"), ("name", "child-b-1"), ("parentTreeId", cs (tshow rootBUuid))]
      _ <- callActionWithParams TreeCreateAction [("ownerType", "note"), ("ownerId", cs (tshow ownerB)), ("nodeType", "file"), ("name", "other-owner-root")]
      _ <- callActionWithParams TreeCreateAction [("ownerType", "strategy"), ("ownerId", cs (tshow ownerA)), ("nodeType", "file"), ("name", "strategy-root")]

      rootsA <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeNote)
        |> filterWhere (#ownerId, ownerA)
        |> filterWhere (#parentTreeId, Nothing)
        |> orderByAsc #nodeOrder
        |> fetch
      map (get #nodeOrder) rootsA `shouldBe` [1, 2]
      map (get #parentTreeId) rootsA `shouldBe` [Nothing, Nothing]

      childrenA <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeNote)
        |> filterWhere (#ownerId, ownerA)
        |> filterWhere (#parentTreeId, Just rootAUuid)
        |> orderByAsc #nodeOrder
        |> fetch
      map (get #nodeOrder) childrenA `shouldBe` [1, 2]
      map (get #parentTreeId) childrenA `shouldBe` [Just rootAUuid, Just rootAUuid]

      childrenB <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeNote)
        |> filterWhere (#ownerId, ownerA)
        |> filterWhere (#parentTreeId, Just rootBUuid)
        |> orderByAsc #nodeOrder
        |> fetch
      map (get #nodeOrder) childrenB `shouldBe` [1]

      rootsOwnerB <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeNote)
        |> filterWhere (#ownerId, ownerB)
        |> filterWhere (#parentTreeId, Nothing)
        |> orderByAsc #nodeOrder
        |> fetch
      map (get #nodeOrder) rootsOwnerB `shouldBe` [1]

      strategyRoots <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeStrategy)
        |> filterWhere (#ownerId, ownerA)
        |> filterWhere (#parentTreeId, Nothing)
        |> orderByAsc #nodeOrder
        |> fetch
      map (get #nodeOrder) strategyRoots `shouldBe` [1]

    it "TreeReadAction filters by owner and returns nodes ordered by node_order" $ withContext do
      ownerA <- mkUuid "00000000-0000-0000-0000-000000000123"
      ownerB <- mkUuid "00000000-0000-0000-0000-000000000124"
      _ <- newTree ownerA "n-3" Nothing 3
      _ <- newTree ownerA "n-1" Nothing 1
      _ <- newTree ownerA "n-2" Nothing 2
      _ <- newTree ownerB "other-owner" Nothing 1
      _ <- newTreeStrategy ownerA "other-type" Nothing 1

      response <- callActionWithParams TreeReadAction [("ownerType", "note"), ("ownerId", cs (tshow ownerA))]
      body <- responseBody response
      let rows = fromMaybe [] (decodeTreeRows body)
      map decodedTreeName rows `shouldBe` ["n-1", "n-2", "n-3"]
      map decodedTreeNodeOrder rows `shouldBe` [1, 2, 3]
      all (\r -> decodedTreeOwnerType r == "note" && decodedTreeOwnerId r == tshow ownerA) rows `shouldBe` True

    it "TreeCreateAction ignores invalid parentTreeId and stores node at root level" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000127"
      invalidParent <- mkUuid "00000000-0000-0000-0000-000000009999"
      _ <- callActionWithParams TreeCreateAction
        [ ("ownerType", "note")
        , ("ownerId", cs (tshow ownerId))
        , ("nodeType", "file")
        , ("name", "orphan")
        , ("parentTreeId", cs (tshow invalidParent))
        ]
      node <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeNote)
        |> filterWhere (#ownerId, ownerId)
        |> filterWhere (#name, "orphan")
        |> fetchOneOrNothing
        >>= expectJust "orphan missing"
      get #parentTreeId node `shouldBe` Nothing

    it "TreeUpdateAction updates name/parent/node_order and keeps name when omitted" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000125"
      a <- newTree ownerId "a" Nothing 1
      b <- newTree ownerId "b" Nothing 2
      let (Id aId) = get #id a
          (Id bId) = get #id b
      _ <- callActionWithParams TreeUpdateAction
        [ ("treeId", cs (tshow bId))
        , ("nodeOrder", "1")
        , ("parentTreeId", cs (tshow aId))
        , ("name", "b-renamed")
        ]
      afterFirst <- fetchOne (get #id b)
      get #name afterFirst `shouldBe` "b-renamed"
      get #parentTreeId afterFirst `shouldBe` Just aId
      get #nodeOrder afterFirst `shouldBe` 1

      _ <- callActionWithParams TreeUpdateAction
        [ ("treeId", cs (tshow bId))
        , ("nodeOrder", "2")
        , ("parentTreeId", cs (tshow aId))
        ]
      afterSecond <- fetchOne (get #id b)
      get #name afterSecond `shouldBe` "b-renamed"
      get #parentTreeId afterSecond `shouldBe` Just aId
      get #nodeOrder afterSecond `shouldBe` 2

    it "TreeUpdateAction rejects self/cycle/cross-owner parents by keeping old parent" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000128"
      otherOwner <- mkUuid "00000000-0000-0000-0000-000000000129"
      root <- newTree ownerId "root" Nothing 1
      let (Id rootId) = get #id root
      child <- newTree ownerId "child" (Just rootId) 1
      let (Id childId) = get #id child
      otherRoot <- newTree otherOwner "other-root" Nothing 1
      let (Id otherRootId) = get #id otherRoot

      _ <- callActionWithParams TreeUpdateAction
        [ ("treeId", cs (tshow rootId))
        , ("nodeOrder", "1")
        , ("parentTreeId", cs (tshow rootId))
        ]
      afterSelf <- fetchOne (get #id root)
      get #parentTreeId afterSelf `shouldBe` Nothing

      _ <- callActionWithParams TreeUpdateAction
        [ ("treeId", cs (tshow rootId))
        , ("nodeOrder", "1")
        , ("parentTreeId", cs (tshow childId))
        ]
      afterCycle <- fetchOne (get #id root)
      get #parentTreeId afterCycle `shouldBe` Nothing

      _ <- callActionWithParams TreeUpdateAction
        [ ("treeId", cs (tshow childId))
        , ("nodeOrder", "1")
        , ("parentTreeId", cs (tshow otherRootId))
        ]
      afterCrossOwner <- fetchOne (get #id child)
      get #parentTreeId afterCrossOwner `shouldBe` Just rootId

    it "TreeDeleteAction deletes subtree nodes and associated cells" $ withContext do
      ownerId <- mkUuid "00000000-0000-0000-0000-000000000126"
      root <- newTree ownerId "root-del" Nothing 1
      let (Id rootId) = get #id root
      child <- newTree ownerId "child-del" (Just rootId) 1
      let (Id childId) = get #id child
      grandChild <- newTree ownerId "grand-del" (Just childId) 1
      let (Id grandId) = get #id grandChild
      keep <- newTree ownerId "keep" Nothing 2
      let (Id keepId) = get #id keep

      _ <- newCell CellOwnerTypeNote rootId 1 "root-cell"
      _ <- newCell CellOwnerTypeNote childId 1 "child-cell"
      _ <- newCell CellOwnerTypeNote grandId 1 "grand-cell"
      _ <- newCell CellOwnerTypeNote keepId 1 "keep-cell"
      _ <- newCell CellOwnerTypeStrategy rootId 1 "strategy-cell-should-keep"

      _ <- callActionWithParams TreeDeleteAction [("treeId", cs (tshow rootId))]

      deletedRoot <- query @Tree |> filterWhere (#id, get #id root) |> fetchOneOrNothing
      deletedChild <- query @Tree |> filterWhere (#id, get #id child) |> fetchOneOrNothing
      deletedGrand <- query @Tree |> filterWhere (#id, get #id grandChild) |> fetchOneOrNothing
      existingKeep <- query @Tree |> filterWhere (#id, get #id keep) |> fetchOneOrNothing
      deletedRoot `shouldBe` Nothing
      deletedChild `shouldBe` Nothing
      deletedGrand `shouldBe` Nothing
      existingKeep `shouldNotBe` Nothing

      rootCells <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, rootId) |> fetch
      childCells <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, childId) |> fetch
      grandCells <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, grandId) |> fetch
      keepCells <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeNote) |> filterWhere (#ownerId, keepId) |> fetch
      strategyCells <- query @Cell |> filterWhere (#ownerType, CellOwnerTypeStrategy) |> filterWhere (#ownerId, rootId) |> fetch
      length rootCells `shouldBe` 0
      length childCells `shouldBe` 0
      length grandCells `shouldBe` 0
      length keepCells `shouldBe` 1
      length strategyCells `shouldBe` 1

mkUuid :: Text -> IO UUID
mkUuid t =
  pure (fromJust (UUID.fromText t))

expectJust :: Text -> Maybe a -> IO a
expectJust _ (Just x) = pure x
expectJust msg Nothing = expectationFailure (cs msg) >> fail (cs msg)

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

data DecodedTree = DecodedTree
  { ownerType :: Text
  , ownerId :: Text
  , name :: Text
  , nodeOrder :: Int
  }

decodeTreeRows :: LBS.ByteString -> Maybe [DecodedTree]
decodeTreeRows body = do
  value <- A.decode body
  arr <- case value of
    A.Array a -> Just (V.toList a)
    _ -> Nothing
  pure (mapMaybe decodeTreeRow arr)

decodeTreeRow :: A.Value -> Maybe DecodedTree
decodeTreeRow (A.Object obj) = do
  ownerType <- lookupText ["owner_type", "ownerType"] obj
  ownerId <- lookupText ["owner_id", "ownerId"] obj
  name <- lookupText ["name"] obj
  nodeOrder <- lookupInt ["node_order", "nodeOrder"] obj
  pure DecodedTree { ownerType, ownerId, name, nodeOrder }
decodeTreeRow _ = Nothing

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

decodedTreeOwnerType :: DecodedTree -> Text
decodedTreeOwnerType DecodedTree { ownerType = x } = x

decodedTreeOwnerId :: DecodedTree -> Text
decodedTreeOwnerId DecodedTree { ownerId = x } = x

decodedTreeName :: DecodedTree -> Text
decodedTreeName DecodedTree { name = x } = x

decodedTreeNodeOrder :: DecodedTree -> Int
decodedTreeNodeOrder DecodedTree { nodeOrder = x } = x
