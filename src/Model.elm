module Model exposing (..)

import Game.Grid as Grid exposing (Grid, Coordinate)
import Game.Tetromino as Tetromino exposing (..)


type alias Model =
    { board : Grid (Maybe Block)
    , activeTetromino : Maybe Tetromino
    }


initialBoard : Grid (Maybe Block)
initialBoard =
    Grid.initialize 10 22 (always Nothing)


init : ( Model, Cmd msg )
init =
    ( { board = initialBoard
      , activeTetromino = Just (Tetromino.create I ( 0, 0 ))
      }
    , Cmd.none
    )
