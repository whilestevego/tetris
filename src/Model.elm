module Model exposing (..)

import Game.Grid as Grid exposing (Grid)
import Game.Tetromino exposing (..)


type alias Model =
    { board : Grid (Maybe Block)
    }


initialBoard : Grid (Maybe Block)
initialBoard =
    Grid.initialize 10 22 (always Nothing)
        |> Grid.set ( 2, 2 ) (Just I)
        |> Grid.set ( 4, 15 ) (Just Z)


init : ( Model, Cmd msg )
init =
    ( { board = initialBoard
      }
    , Cmd.none
    )
