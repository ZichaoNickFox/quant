module GHC.Types
  ( Float
  , List
  ) where

import Prim (Array, Number)

-- | Compatibility aliases so purescript-bridge generated code compiles.
-- | purescript-bridge currently emits `Float` and `List` from `GHC.Types`;
-- | map them to PureScript's `Number` and `Array`.
type Float = Number
type List = Array
