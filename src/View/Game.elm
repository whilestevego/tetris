module View.Game exposing (viewGame)

import Game.Grid as Grid exposing (Grid, Coordinate)
import Game.Tetromino as Tetromino exposing (..)
import Html exposing (Html)
import Model exposing (Model)
import Svg exposing (Svg, Attribute, rect, g, svg)
import Svg.Attributes
    exposing
        ( fill
        , height
        , stroke
        , strokeWidth
        , viewBox
        , width
        , x
        , y
        )


px : Int -> String
px int =
    (toString int) ++ "px"


u : Int -> String
u int =
    toString int


blockSize : Int
blockSize =
    30


viewGame : Model -> Html msg
viewGame { board, activeTetromino } =
    let
        columns =
            Grid.columnLength board

        rows =
            Grid.rowLength board
    in
        svg
            [ viewBox
                ([ 0, 0, columns * blockSize, rows * blockSize ]
                    |> List.map u
                    |> String.join " "
                )
            , width (u <| columns * blockSize)
            , height (u <| rows * blockSize)
            ]
            (case activeTetromino of
                Just tetromino ->
                    [ viewBoard board, viewTetromino tetromino ]

                Nothing ->
                    [ viewBoard board ]
            )


viewBoard : Grid (Maybe Block) -> Html msg
viewBoard board =
    let
        columns =
            Grid.columnLength board

        rows =
            Grid.rowLength board
    in
        g []
            [ rect
                [ width (u (columns * blockSize))
                , height (u (rows * blockSize))
                , stroke "gainsboro"
                , strokeWidth "2"
                , fill "none"
                ]
                []
            , viewBlocks ( 0, 0 ) board
            ]


viewBlocks : Coordinate -> Grid (Maybe Block) -> Svg msg
viewBlocks origin grid =
    g []
        (grid
            |> Grid.coordinateMap (blockMapper origin)
            |> Grid.toFlatList
            |> List.filterMap (Maybe.map identity)
        )


blockMapper : Coordinate -> Coordinate -> Maybe Block -> Maybe (Svg msg)
blockMapper ( oX, oY ) ( pX, pY ) block =
    block
        |> Maybe.map
            (\x ->
                viewBlock
                    [ fill (Tetromino.color x) ]
                    ( oX + pX, oY + pY )
            )


viewBlock : List (Attribute msg) -> Coordinate -> Html msg
viewBlock styles ( col, row ) =
    rect
        ([ width (u blockSize)
         , height (u blockSize)
         , x (u (blockSize * col))
         , y (u (blockSize * row))
         ]
            ++ styles
        )
        []


viewTetromino : Tetromino -> Svg msg
viewTetromino { blocks, position } =
    viewBlocks position blocks
