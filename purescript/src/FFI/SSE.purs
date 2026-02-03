module FFI.SSE (attachEventSource) where

import Prelude

import Effect (Effect)

foreign import attachEventSource :: String -> (String -> Effect Unit) -> Effect Unit
