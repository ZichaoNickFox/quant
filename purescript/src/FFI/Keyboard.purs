module FFI.Keyboard (isEnterKey) where

import Web.Event.Event (Event)

foreign import isEnterKey :: Event -> Boolean
