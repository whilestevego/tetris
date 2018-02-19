module Model exposing (..)

import Game.Grid as Grid exposing (Grid)


type alias Model =
    { board : Grid Int
    }


init : ( Model, Cmd msg )
init =
    ( { board = Grid.initialize 10 22 (always 0)
      }
    , Cmd.none
    )
