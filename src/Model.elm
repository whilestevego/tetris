module Model exposing (..)

import Game.Grid as Grid exposing (Grid, Coordinate)
import Game.Tetromino as Tetromino exposing (..)


type Msg
    = AnimationDiffs Float
    | GetRandom Int


type alias Model =
    { activeTetromino : Maybe Tetromino
    , board : Grid (Maybe Block)
    , dt : Float
    , tick : Int
    , randomNum : Int
    }


initialBoard : Grid (Maybe Block)
initialBoard =
    Grid.initialize 10 22 (always Nothing)


init : ( Model, Cmd msg )
init =
    ( { activeTetromino = Nothing
      , board = initialBoard
      , dt = 0
      , tick = 0
      , randomNum = 0
      }
    , Cmd.none
    )
