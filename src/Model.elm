module Model exposing (..)

import Game.Grid as Grid exposing (Grid, Coordinate)
import Game.Tetromino as Tetromino exposing (..)
import Keyboard exposing (KeyCode)
import Set exposing (Set)


type Msg
    = AnimationDiffs Float
    | GetRandom Int
    | KeyDown KeyCode
    | KeyUp KeyCode


type alias Model =
    { activeTetromino : Maybe Tetromino
    , board : Grid (Maybe Block)
    , dt : Float
    , tick : Int
    , randomNum : Int
    , inputMods : Set KeyCode
    , inputKey : Maybe KeyCode
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
      , inputMods = Set.empty
      , inputKey = Nothing
      }
    , Cmd.none
    )
