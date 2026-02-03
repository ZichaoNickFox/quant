module Test.Proto.TypesSpec (tests) where

import Data.Array (length)
import GHC.Types (Float, List)
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "GHC.Types aliases compile and behave as expected" do
    let x :: Float
        x = 1.5
        xs :: List Int
        xs = [1, 2, 3]
    x `shouldEqual` 1.5
    length xs `shouldEqual` 3
