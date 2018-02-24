module View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Model exposing (Model)
import View.Game exposing (viewGame)


view : Model -> Html msg
view model =
    let
        css =
            style
                [ ( "display", "flex" )
                , ( "alignItems", "center" )
                , ( "justifyContent", "center" )
                , ( "width", "100vw" )
                , ( "height", "100vh" )
                ]
    in
        div [ css ]
            [ viewGame model
            ]
