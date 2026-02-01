module Main where
import IHP.Prelude

import Config
import qualified IHP.Server

import Web.FrontController
import Web.Prelude
import Web.Types
import Web.Workers

instance FrontController RootApplication where
  controllers = [ mountFrontController WebApplication ]

instance Worker RootApplication where
  workers _ = workers WebApplication

main :: IO ()
main = IHP.Server.run config
