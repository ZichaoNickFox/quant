module FRP.Combinator
  ( merge
  , map
  , filter
  , scan
  , combineLatest2
  , withLatestFrom
  , pairwise
  , take1
  ) where

import Prelude

import Control.Alternative ((<|>))
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Functor as Functor
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import FRP.Event (Event, mapAccum)

-- Operators (thin wrappers over Event primitives).
merge :: forall a. Event a -> Event a -> Event a
merge = (<|>)

map :: forall a b. (a -> b) -> Event a -> Event b
map = Functor.map

filter :: forall a. (a -> Boolean) -> Event a -> Event a
filter p = filterMap (\a -> if p a then Just a else Nothing)

scan :: forall s a b. (a -> s -> Tuple s b) -> s -> Event a -> Event b
scan step init e = mapAccum step e init

take1 :: forall a. Event a -> Event a
take1 e =
  let
    step a fired =
      let shouldEmit = not fired
          fired' = fired || shouldEmit
      in Tuple fired' (if shouldEmit then Just a else Nothing)
  in
    filterMap identity $
      scan step false e

-- combineLatest for two streams: emits when both have produced at least once.
combineLatest2 :: forall a b. Event a -> Event b -> Event (Tuple a b)
combineLatest2 a b =
  let
    toSignal = merge (Left <$> a) (Right <$> b)
    step sig state =
      let state' = case sig of
            Left av -> state { aVal = Just av }
            Right bv -> state { bVal = Just bv }
      in case state' of
          { aVal: Just av, bVal: Just bv } -> Tuple state' (Just (Tuple av bv))
          _ -> Tuple state' Nothing
  in
    filterMap identity $
      scan step { aVal: Nothing, bVal: Nothing } toSignal

-- RxJS-style: emit only when source emits and the other stream has produced at least once.
withLatestFrom :: forall a b. Event a -> Event b -> Event (Tuple a b)
withLatestFrom source other =
  let
    toSignal = merge (Left <$> source) (Right <$> other)
    step sig state =
      case sig of
        Right b -> Tuple { latest: Just b } Nothing
        Left a -> Tuple state (Tuple a <$> state.latest)
  in
    filterMap identity $
      scan step { latest: Nothing } toSignal

-- RxJS-style: emit previous/current pair after the second value.
pairwise :: forall a. Event a -> Event (Tuple a a)
pairwise e =
  let
    step a prev =
      let out = case prev of
            Just p -> Just (Tuple p a)
            Nothing -> Nothing
      in Tuple (Just a) out
  in
    filterMap identity $
      scan step Nothing e
