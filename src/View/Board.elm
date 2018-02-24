module View.Board exposing (viewBoard)

import Game.Grid as Grid exposing (Grid, Coordinate)
import Game.Tetromino as Tetromino exposing (..)
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


viewBlockGrid : Grid (Maybe Block) -> Svg msg
viewBlockGrid grid =
    g []
        (grid
            |> Grid.coordinateMap viewBlock
            |> Grid.toFlatList
            |> List.filterMap (Maybe.map identity)
        )


viewBlock : Coordinate -> Maybe Block -> Maybe (Svg msg)
viewBlock coords block =
    block
        |> Maybe.map
            (\x -> viewBaseBlock [ fill (Tetromino.color x) ] coords)


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
