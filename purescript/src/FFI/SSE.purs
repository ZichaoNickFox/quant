module FFI.SSE (attachEventSource) where

import Effect (Effect)

import Prelude

foreign import attachEventSource :: String -> (String -> Effect Unit) -> Effect Unit
