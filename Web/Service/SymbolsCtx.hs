{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Service.SymbolsCtx
  ( SymbolsCtx(..)
  ) where

import           Data.Typeable (Typeable)
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Proto.Symbols as Proto
import           Proto.SseStatus (SseStatus)
import           Web.Prelude
import           Web.Repo.SymbolRepo (getSymbolsByTypeMap, upsertSymbols)
import           Web.Fetcher.SymbolFetcher (downloadSymbols)
import           Web.Service.Policy.CoveragePolicy
import           Web.Service.Policy.RepoPolicy
import           Web.Service.Policy.RespondPolicy
import           Web.Service.Policy.FetchPolicy (FetchPolicy(..))
import           Web.Service.Policy.TTLPolicy
import           Web.Types
import qualified Web.Service.Process.TTLProcess as TTLProcess

data SymbolsCtx = SymbolsCtx
  { clientId :: Text
  } deriving (Eq, Show, Typeable)

------------------
instance CoveragePolicy SymbolsCtx where
  coveredByRequest a b = clientId a == clientId b
  coveredByRepo ctx = not <$> TTLProcess.needTTL ctx

------------------
instance FetchPolicy SymbolsCtx where
  type FetchResult SymbolsCtx = [Symbol]
  fetchTask _ = do
    let sts = [minBound .. maxBound] :: [SymbolType]
    concat <$> forM sts downloadSymbols

------------------
instance RepoPolicy SymbolsCtx where
  type RepoPayload SymbolsCtx = Proto.APISymbolsResponse
  readFromRepo _ = do
    symbolsByType <- getSymbolsByTypeMap
    let toInfo sym = Proto.SymbolInfo
          { symbolType = T.toLower (tshow (get #symbolType sym))
          , code = get #code sym
          , name = get #name sym
          }
        allSymbols = concatMap (map toInfo) (M.elems symbolsByType)
    pure $ Proto.APISymbolsResponse
      { complete = True
      , symbols = allSymbols
      }
  upsertFromFetch ctx symbols = do
    upsertSymbols symbols
    TTLProcess.upsertTTL ctx

------------------
instance TTLPolicy SymbolsCtx where
  ttlKey _ = "symbols"
  ttlSeconds _ = 604800

------------------
instance RespondPolicy SymbolsCtx where
  type RespondPayload SymbolsCtx = Proto.APISymbolsResponse
  respondHttp _ Proto.APISymbolsResponse { symbols } =
    A.object
      ( [ "symbols" A..= symbols
        ]
      )
  respondSse _ status =
    A.object [ "status" A..= status ]
