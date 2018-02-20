module Model exposing (..)

import Game.Grid as Grid exposing (Grid)
import Game.Tetromino exposing (..)


type alias Model =
    { board : Grid (Maybe Block)
    }


initialBoard : Grid (Maybe Block)
initialBoard =
    Grid.initialize 10 22 (always Nothing)
        |> Grid.mergeAt (tetrominoI |> Grid.rotate90) ( 3, -1 )
        |> Grid.mergeAt (tetrominoJ |> Grid.rotate90) ( 3, 10 )
        |> Grid.mergeAt (tetrominoL |> Grid.rotate180) ( 5, 14 )


init : ( Model, Cmd msg )
init =
    ( { board = initialBoard
      }
    , Cmd.none
    )
