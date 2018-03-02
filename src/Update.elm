module Update exposing (..)

import Game.Tetromino as T exposing (Tetromino, Block(..))
import Model exposing (..)
import Time exposing (Time)
import Game.Grid as Grid exposing (Grid)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationDiffs dt ->
            ( updateTime dt model, Cmd.none )


updateTime : Float -> Model -> Model
updateTime dt model =
    let
        nextDt =
            model.dt + dt

        nextTick =
            nextDt |> Time.inSeconds |> ((*) 1.5) |> truncate

        mapper =
            if (nextTick > model.tick) then
                gameTick
            else
                identity
    in
        mapper
            { model
                | dt = nextDt
                , tick = nextTick
            }


(?>) : Maybe a -> (a -> b) -> Maybe b
(?>) =
    flip Maybe.map
infixr 9 ?>


gameTick : Model -> Model
gameTick model =
    let
        { tick, activeTetromino, board } =
            model
    in
        case activeTetromino of
            Nothing ->
                { model
                    | activeTetromino = Just (T.create L ( 4, -1 ))
                }

            Just tetro ->
                if detectCollision (tetro |> T.moveDown) board then
                    { model
                        | activeTetromino = Nothing
                        , board = Grid.mergeAt tetro.blocks tetro.position board
                    }
                else
                    { model
                        | activeTetromino = tetro |> T.moveDown |> Just
                    }


detectCollision : Tetromino -> Grid (Maybe Block) -> Bool
detectCollision tetro board =
    tetro
        |> T.toPositionList
        |> List.any (\pos -> board |> Grid.isNothing pos |> not)
