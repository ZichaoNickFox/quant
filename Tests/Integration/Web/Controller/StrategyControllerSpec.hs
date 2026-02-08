{-# LANGUAGE OverloadedStrings #-}
module Tests.Integration.Web.Controller.StrategyControllerSpec (tests) where

import Config (config)
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (callActionWithParams, withContext)
import Prelude
import Test.Hspec
import Web.Controller.StrategyController ()
import Web.FrontController ()
import Web.Prelude
import Web.Types
import qualified Web.Types as Types

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "StrategyController (integration)" do
    it "StrategyCreateAction creates strategy and inserts root tree node" $ withContext do
      _ <- callActionWithParams StrategyCreateAction []

      strategies <- query @Strategy |> fetch
      length strategies `shouldBe` 1

      created <- case strategies of
        [x] -> pure x
        _ -> expectationFailure "expected exactly one strategy" >> fail "strategy count mismatch"
      let (Id strategyId) = get #id created

      strategyTrees <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeStrategy)
        |> filterWhere (#ownerId, strategyId)
        |> orderByAsc #nodeOrder
        |> fetch

      length strategyTrees `shouldBe` 1
      rootNode <- case strategyTrees of
        [x] -> pure x
        _ -> expectationFailure "expected exactly one strategy root tree node" >> fail "tree count mismatch"
      get #parentTreeId rootNode `shouldBe` Nothing
      get #nodeOrder rootNode `shouldBe` 1
      get #nodeType rootNode `shouldBe` Types.File

    it "StrategyTreeCreateAction under a parent creates a child strategy node with a new owner_id" $ withContext do
      beforeStrategies <- query @Strategy |> fetch
      let beforeStrategyIds = map (get #id) beforeStrategies
      _ <- callActionWithParams StrategyCreateAction []

      rootStrategy <- do
        strategies <- query @Strategy |> fetch
        let newOnes = filter (\s -> get #id s `notElem` beforeStrategyIds) strategies
        case newOnes of
          [x] -> pure x
          _ -> expectationFailure "expected exactly one newly created root strategy" >> fail "strategy diff mismatch"
      let (Id rootStrategyId) = get #id rootStrategy

      rootNode <- do
        roots <- query @Tree
          |> filterWhere (#ownerType, TreeOwnerTypeStrategy)
          |> filterWhere (#ownerId, rootStrategyId)
          |> filterWhere (#parentTreeId, Nothing)
          |> fetch
        case roots of
          [x] -> pure x
          _ -> expectationFailure "expected exactly one root tree node" >> fail "root tree count mismatch"

      _ <- callActionWithParams
        StrategyTreeCreateAction
        [ ("strategyId", cs (tshow rootStrategyId))
        , ("parentTreeId", cs (tshow (get #id rootNode)))
        ]

      children <- query @Tree
        |> filterWhere (#parentTreeId, Just (let (Id u) = get #id rootNode in u))
        |> orderByAsc #nodeOrder
        |> fetch
      length children `shouldBe` 1

      child <- case children of
        [x] -> pure x
        _ -> expectationFailure "expected exactly one child tree node" >> fail "child tree count mismatch"
      let childOwnerId = get #ownerId child

      childOwnerId `shouldNotBe` rootStrategyId

      childStrategy <- query @Strategy |> filterWhere (#id, Id childOwnerId) |> fetchOneOrNothing
      case childStrategy of
        Nothing -> expectationFailure "expected child strategy to exist"
        Just _ -> pure ()
