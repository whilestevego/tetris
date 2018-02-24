module Main exposing (..)

import Html exposing (Html)
import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import AnimationFrame
import Time exposing (Time)


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
