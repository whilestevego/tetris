module Model exposing (..)

import Grid exposing (Grid)


type alias Model =
    { board : Grid Int
    }


init : ( Model, Cmd msg )
init =
    ( { board = Grid.initialize 10 22 (always 0)
      }
    , Cmd.none
    )
