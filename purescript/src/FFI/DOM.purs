module FFI.DOM (getQueryParam, setInnerHTMLById) where

import Data.Maybe (Maybe)
import Effect (Effect)

foreign import setInnerHTMLById :: String -> String -> Effect Boolean
foreign import getQueryParam :: String -> Effect (Maybe String)
