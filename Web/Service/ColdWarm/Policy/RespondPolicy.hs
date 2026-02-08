{-# LANGUAGE TypeFamilies #-}
module Web.Service.ColdWarm.Policy.RespondPolicy
  ( RespondPolicy(..)
  ) where

import qualified Data.Aeson as A
import           Proto.SseStatus (SseStatus)
import           Web.Prelude

class RespondPolicy ctx where
  type RespondPayload ctx
  respondHttp :: ctx -> Bool -> RespondPayload ctx -> A.Value
  respondSse :: ctx -> SseStatus -> A.Value
