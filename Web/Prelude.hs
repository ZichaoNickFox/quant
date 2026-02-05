{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Web.Prelude
  ( logController
  , logDebug
  , logError
  , logFetch
  , logInfo
  , logWarn
  , requestUrl
  , module IHP.AutoRefresh
  , module IHP.AutoRefresh.View
  , module IHP.Controller.Layout
  , module IHP.Controller.Param
  , module IHP.Controller.Render
  , module IHP.ControllerPrelude
  , module IHP.ControllerSupport
  , module IHP.Fetch
  , module IHP.FrameworkConfig
  , module IHP.HaskellSupport
  , module IHP.Log
  , module IHP.Log.Types
  , module IHP.LoginSupport.Middleware
  , module IHP.Modal.Types
  , module IHP.Modal.ViewFunctions
  , module IHP.ModelSupport
  , module IHP.RouterPrelude
  , module IHP.RouterSupport
  , module IHP.PageHead.ViewFunctions
  , module IHP.Prelude
  , module IHP.QueryBuilder
  , module IHP.ViewSupport
  , module Network.Wai
  ) where

import qualified Data.Text as T
import           Data.Char
import           IHP.AutoRefresh
import           IHP.AutoRefresh.View
import           IHP.Controller.Layout
import           IHP.Controller.Param
import           IHP.Controller.Render
import           IHP.ControllerPrelude hiding (Error, Symbol)
import           IHP.ControllerSupport
import           IHP.Fetch
import           IHP.FrameworkConfig
import           IHP.HaskellSupport
import           IHP.Log hiding (debug, error, Error, info, warn)
import qualified IHP.Log as Log (debug, error, info, warn)
import           IHP.Log.Types
import           IHP.LoginSupport.Middleware hiding (Symbol)
import           IHP.Modal.Types
import           IHP.Modal.ViewFunctions
import           IHP.ModelSupport
import           IHP.RouterPrelude ()
import           IHP.RouterSupport hiding (get)
import           IHP.PageHead.ViewFunctions (pageTitleOrDefault)
import           IHP.Prelude hiding (Symbol)
import           IHP.QueryBuilder
import           IHP.ViewSupport hiding (fetch, param, query)
import           Network.Wai
import qualified System.Log.FastLogger as FastLogger

logDebug :: (?context :: context, LoggingProvider context) => Text -> IO ()
logDebug = Log.debug
logError :: (?context :: context, LoggingProvider context) => Text -> IO ()
logError = Log.error
logInfo :: (?context :: context, LoggingProvider context) => Text -> IO ()
logInfo = Log.info
logWarn :: (?context :: context, LoggingProvider context) => Text -> IO ()
logWarn = Log.warn

logController :: (?context :: context, LoggingProvider context) => LogLevel -> Text -> IO ()
logController level log = logInfo $ "\\ESC[36m" <> log <> "\\ESC[0m"

logFetch :: (?context :: context, LoggingProvider context) => Text -> IO ()
logFetch log = logInfo $ "\\ESC[32m" <> log <> "\\ESC[0m"

requestUrl :: (?context :: ControllerContext ,?modelContext :: ModelContext ,?theAction :: controller ) => Text
requestUrl = "rawPath=" <> tshow (rawPathInfo request) <> " " <> "rawQuery=" <> tshow (rawQueryString request)
