module View.Board exposing (viewBoard)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Grid exposing (Grid, Coordinate)


px : Int -> String
px int =
    (toString int) ++ "px"


u : Int -> String
u int =
    toString int


viewBoard : Int -> Grid Int -> Html msg
viewBoard blockSize grid =
    let
        columns =
            Grid.columns grid

        rows =
            Grid.rows grid
    in
        svg
            [ width (u <| columns * blockSize)
            , height (u <| rows * blockSize)
            , viewBox
                ([ 0, 0, columns * blockSize, rows * blockSize ]
                    |> List.map u
                    |> String.join " "
                )
            ]
            [ g []
                (grid
                    |> Grid.coordinateMap (\coords _ -> viewBlock blockSize coords)
                    |> Grid.toList
                    |> List.concatMap (identity)
                )
            ]


viewBlock : Int -> Coordinate -> Html msg
viewBlock blockSize ( col, row ) =
    rect
        [ x (u <| col * blockSize)
        , y (u <| row * blockSize)
        , width (u blockSize)
        , height (u blockSize)
        , fill "whitesmoke"
        ]
        []
