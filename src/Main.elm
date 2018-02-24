module Main exposing (..)

import Html exposing (Html)
import Model exposing (Model, init)
import View exposing (view)
import AnimationFrame
import Time exposing (Time)
import Game.Tetromino as Tetromino


---- UPDATE ----


type Msg
    = AnimationDiffs Float


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



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs (AnimationDiffs << Time.inMilliseconds << identity)
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
