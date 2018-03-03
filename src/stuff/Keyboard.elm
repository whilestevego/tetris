module Stuff.Keyboard exposing (..)

import Keyboard exposing (KeyCode)
import Set exposing (Set)


modifiers : Set KeyCode
modifiers =
    -- Modifers are Shift | Control | Alt | Meta
    Set.fromList [ 16, 17, 18, 91 ]
