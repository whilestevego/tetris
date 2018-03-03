module Main exposing (..)

import Html exposing (Html)
import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import AnimationFrame
import Time exposing (Time)
import Keyboard exposing (..)


---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs
            (AnimationDiffs << Time.inMilliseconds << identity)
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
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
