module TetrominoTests exposing (..)

import Test exposing (..)
import Expect
import Game.Tetromino as Tetromino exposing (..)
import Game.Grid as Grid


(?) : Maybe a -> a -> a
(?) =
    flip Maybe.withDefault


all : Test
all =
    describe "Tetromino"
        [ describe "create"
            [ test "to have correct blocks" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 )
                    in
                        Expect.equal
                            tetro.blocks
                            (Grid.fromList
                                [ [ Nothing, Nothing, Nothing ]
                                , [ Just J, Nothing, Nothing ]
                                , [ Just J, Just J, Just J ]
                                ]
                            )
            , test "to have correct top-left bound" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 )
                    in
                        Expect.equal
                            (Tuple.first tetro.bounds)
                            ( 0, 1 )
            , test "to have correct bottom-right bound" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 )
                    in
                        Expect.equal
                            (Tuple.second tetro.bounds)
                            ( 2, 2 )
            ]
        , describe "rotateRight"
            [ test "to have correct blocks" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 ) |> rotateRight
                    in
                        Expect.equal
                            tetro.blocks
                            (Grid.fromList
                                [ [ Just J, Just J, Nothing ]
                                , [ Just J, Nothing, Nothing ]
                                , [ Just J, Nothing, Nothing ]
                                ]
                            )
            , test "to update top-left bound" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 ) |> rotateRight
                    in
                        Expect.equal
                            (Tuple.first tetro.bounds)
                            ( 0, 0 )
            , test "to have correct bottom-right bound" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 ) |> rotateRight
                    in
                        Expect.equal
                            (Tuple.second tetro.bounds)
                            ( 1, 2 )
            ]
        , describe "getBottomBound"
            [ test "with (0, 0) position" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 )
                    in
                        Expect.equal (getBottomBound tetro) 2
            , test "with (3, 5) position" <|
                \_ ->
                    let
                        tetro =
                            create J ( 3, 5 )
                    in
                        Expect.equal (getBottomBound tetro) 7
            ]
        , describe "getRightBound"
            [ test "with (0, 0) position" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 )
                    in
                        Expect.equal (getRightBound tetro) 2
            , test "with (3, 5) position" <|
                \_ ->
                    let
                        tetro =
                            create J ( 3, 5 )
                    in
                        Expect.equal (getRightBound tetro) 5
            ]
        , describe "coordinateFoldl"
            [ test "over positions at (0, 0)" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 )
                    in
                        Expect.equal
                            (coordinateFoldl
                                (\( x, y ) _ acc ->
                                    toString x ++ toString y ++ "." ++ acc
                                )
                                ""
                                tetro
                            )
                            "22.12.02.21.11.01.20.10.00."
            , test "over positions at (1, 2)" <|
                \_ ->
                    let
                        tetro =
                            create J ( 1, 2 )
                    in
                        Expect.equal
                            (coordinateFoldl
                                (\( x, y ) _ acc ->
                                    toString x ++ toString y ++ "." ++ acc
                                )
                                ""
                                tetro
                            )
                            "34.24.14.33.23.13.32.22.12."
            ]
        , describe "toPositionList"
            [ test "over positions at (0, 0)" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 )
                    in
                        Expect.equal
                            (tetro |> toPositionList)
                            [ ( 2, 2 ), ( 1, 2 ), ( 0, 2 ), ( 0, 1 ) ]
            , test "over positions at (1, 2)" <|
                \_ ->
                    let
                        tetro =
                            create J ( 1, 2 )
                    in
                        Expect.equal
                            (tetro |> toPositionList)
                            [ ( 3, 4 ), ( 2, 4 ), ( 1, 4 ), ( 1, 3 ) ]
            ]
        ]
