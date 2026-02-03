{-# LANGUAGE TypeFamilies #-}
module Web.Service.Policy.RespondPolicy
  ( RespondPolicy(..)
  ) where

import qualified Data.Aeson as A
import           Web.Prelude

class RespondPolicy ctx where
  type RespondPayload ctx
  respondHttp :: ctx -> RespondPayload ctx -> A.Value
  respondSse :: ctx -> Bool -> A.Value
