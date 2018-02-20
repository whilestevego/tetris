module View.Board exposing (viewBoard)

import Game.Grid as Grid exposing (Grid, Coordinate)
import Game.Tetromino exposing (..)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


px : Int -> String
px int =
    (toString int) ++ "px"


u : Int -> String
u int =
    toString int


blockSize : Int
blockSize =
    30


viewBoard : Grid (Maybe Block) -> Html msg
viewBoard grid =
    let
        columns =
            Grid.columnLength grid

        rows =
            Grid.rowLength grid
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
            [ viewBlockGrid grid
            ]


viewBlockGrid : Grid (Maybe Block) -> Html msg
viewBlockGrid grid =
    g []
        (grid
            |> Grid.coordinateMap viewBlock
            |> Grid.toList
            |> List.concatMap (identity)
        )


viewBlock : Coordinate -> Maybe Block -> Html msg
viewBlock coords block =
    case block of
        Just x ->
            case x of
                I ->
                    viewBaseBlock [ fill "#E91E63" ] coords

                O ->
                    viewBaseBlock [ fill "#673AB7" ] coords

                T ->
                    viewBaseBlock [ fill "#2196F3" ] coords

                J ->
                    viewBaseBlock [ fill "#00BCD4" ] coords

                L ->
                    viewBaseBlock [ fill "#4CAF50" ] coords

                S ->
                    viewBaseBlock [ fill "#CDDC39" ] coords

                Z ->
                    viewBaseBlock [ fill "#FFC107" ] coords

        Nothing ->
            viewBaseBlock [ fill "#F5F5F5" ] coords


viewBaseBlock : List (Attribute msg) -> Coordinate -> Html msg
viewBaseBlock styles ( col, row ) =
    rect
        ([ width (u blockSize)
         , height (u blockSize)
         , x (u (blockSize * col))
         , y (u (blockSize * row))
         ]
            ++ styles
        )
        []
