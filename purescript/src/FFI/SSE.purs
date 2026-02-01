module FFI.SSE (attachEventSource) where

import Prelude

import Effect (Effect)

foreign import attachEventSource :: String -> (Unit -> Effect Unit) -> Effect Unit
