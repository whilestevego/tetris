module Update exposing (..)

import Game.Tetromino as Tetromino
import Model exposing (..)
import Time exposing (Time)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationDiffs dt ->
            ( updateTime dt model, Cmd.none )


updateTime : Float -> Model -> Model
updateTime dt model =
    let
        newDt =
            model.dt + dt

        newTick =
            newDt |> Time.inSeconds |> truncate

        mapper =
            if (newTick > model.tick) then
                gameTick
            else
                identity
    in
        mapper
            { model
                | dt = newDt
                , tick = newTick
            }


gameTick : Model -> Model
gameTick model =
    let
        { tick, activeTetromino, board } =
            model
    in
        { model
            | activeTetromino = Maybe.map Tetromino.moveDown activeTetromino
        }
