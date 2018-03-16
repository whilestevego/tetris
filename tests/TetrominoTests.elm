module TetrominoTests exposing (..)

import Test exposing (..)
import Expect
import Game.Tetromino as Tetromino exposing (..)
import Game.Grid as Grid
import Set exposing (Set)


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
                                [ [ Just J, Nothing, Nothing ]
                                , [ Just J, Just J, Just J ]
                                , [ Nothing, Nothing, Nothing ]
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
                            ( 0, 0 )
            , test "to have correct bottom-right bound" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 )
                    in
                        Expect.equal
                            (Tuple.second tetro.bounds)
                            ( 2, 1 )
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
                                [ [ Nothing, Just J, Just J ]
                                , [ Nothing, Just J, Nothing ]
                                , [ Nothing, Just J, Nothing ]
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
                            ( 1, 0 )
            , test "to have correct bottom-right bound" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 ) |> rotateRight
                    in
                        Expect.equal
                            (Tuple.second tetro.bounds)
                            ( 2, 2 )
            ]
        , describe "getBottomBound"
            [ test "with (0, 0) position" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 )
                    in
                        Expect.equal (getBottomBound tetro) 1
            , test "with (3, 5) position" <|
                \_ ->
                    let
                        tetro =
                            create J ( 3, 5 )
                    in
                        Expect.equal (getBottomBound tetro) 6
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
                            [ ( 2, 1 ), ( 1, 1 ), ( 0, 1 ), ( 0, 0 ) ]
            , test "over positions at (1, 2)" <|
                \_ ->
                    let
                        tetro =
                            create J ( 1, 2 )
                    in
                        Expect.equal
                            (tetro |> toPositionList)
                            [ ( 3, 3 ), ( 2, 3 ), ( 1, 3 ), ( 1, 2 ) ]
            ]
        , describe "toPositionSet"
            [ test "over positions at (0, 0)" <|
                \_ ->
                    let
                        tetro =
                            create J ( 0, 0 )
                    in
                        Expect.equal
                            (tetro |> toPositionSet)
                            (Set.fromList
                                [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
                            )
            , test "over positions at (1, 2)" <|
                \_ ->
                    let
                        tetro =
                            create J ( 1, 2 )
                    in
                        Expect.equal
                            (tetro |> toPositionSet)
                            (Set.fromList
                                [ ( 1, 2 ), ( 1, 3 ), ( 2, 3 ), ( 3, 3 ) ]
                            )
            ]
        , describe "getOccupiedRows"
            [ test "with J-Tetromino at (0, 0)" <|
                \_ ->
                    Expect.equal
                        (create J ( 0, 0 ) |> getOccupiedRows)
                        (Set.fromList [ 0, 1 ])
            , test "with J-Tetromino at (4, 6)" <|
                \_ ->
                    Expect.equal
                        (create J ( 4, 6 ) |> getOccupiedRows)
                        (Set.fromList [ 6, 7 ])
            , test "with I-Tetromino at (3, 3)" <|
                \_ ->
                    Expect.equal
                        (create I ( 3, 3 ) |> getOccupiedRows)
                        (Set.fromList [ 4 ])
            , test "with I-Tetromino at (0, 0) rotated right" <|
                \_ ->
                    Expect.equal
                        (create I ( 0, 0 ) |> rotateRight |> getOccupiedRows)
                        (Set.fromList [ 0, 1, 2, 3 ])
            ]
        , describe "getOccupiedColumns"
            [ test "with J-Tetromino at (0, 0)" <|
                \_ ->
                    Expect.equal
                        (create J ( 0, 0 ) |> getOccupiedColumns)
                        (Set.fromList [ 0, 1, 2 ])
            , test "with J-Tetromino at (4, 6)" <|
                \_ ->
                    Expect.equal
                        (create J ( 4, 6 ) |> getOccupiedColumns)
                        (Set.fromList [ 4, 5, 6 ])
            , test "with I-Tetromino at (3, 3)" <|
                \_ ->
                    Expect.equal
                        (create I ( 3, 3 ) |> getOccupiedColumns)
                        (Set.fromList [ 3, 4, 5, 6 ])
            , test "with I-Tetromino at (0, 0) rotated right" <|
                \_ ->
                    Expect.equal
                        (create I ( 0, 0 ) |> rotateRight |> getOccupiedColumns)
                        (Set.fromList [ 2 ])
            ]
        ]
