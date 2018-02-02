module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class, src, style)
import Grid exposing (Grid)


---- MODEL ----


type alias Model =
    { board : Grid Int
    }


init : ( Model, Cmd Msg )
init =
    ( { board = Grid.initialize 10 22 (always 0)
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- H E L P E R S ----


px : Int -> String
px int =
    (toString int) ++ "px"


translate : String -> String -> String
translate x y =
    [ "translate(", x, ",", y, ")" ] |> String.join ""



---- V I E W ----


view : Model -> Html Msg
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
            [ viewBoard model.board
            ]


blockSize : Int
blockSize =
    25


viewBoard : Grid Int -> Html Msg
viewBoard grid =
    let
        css =
            style
                [ ( "position", "relative" )
                , ( "width", px ((Grid.columns grid) * blockSize) )
                , ( "height", px ((Grid.rows grid) * blockSize) )
                , ( "boxSizing", "content-box" )
                , ( "border", "thin solid gainsboro" )
                ]
    in
        div [ class "Board", css ]
            (grid
                |> Grid.coordinateMap (\coords _ -> viewBlock coords)
                |> Grid.toList
                |> List.concatMap (identity)
            )


viewBlock : Grid.Coordinate -> Html Msg
viewBlock ( col, row ) =
    let
        css =
            style
                [ ( "width", px blockSize )
                , ( "height", px blockSize )
                , ( "backgroundColor", "whitesmoke" )
                , ( "border", "thin solid gainsboro" )
                , ( "position", "absolute" )
                , ( "transform"
                  , translate (px (col * blockSize)) (px (row * blockSize))
                  )
                ]
    in
        div [ class "Block", css ] []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
