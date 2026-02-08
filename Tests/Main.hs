module Main where

import Prelude
import Test.Hspec

import qualified Tests.Architecture.Main as Architecture
import qualified Tests.Integration.Main as Integration
import qualified Tests.Unit.Main as Unit

main :: IO ()
main = hspec do
  Architecture.tests
  Unit.tests
  Integration.tests
