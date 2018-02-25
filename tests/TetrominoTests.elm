module TetrominoTests exposing (..)

import Test exposing (..)
import Expect
import Game.Tetromino as Tetromino exposing (..)
import Game.Grid as Grid


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
                                , [ Nothing, Just S, Just S ]
                                , [ Just S, Just S, Nothing ]
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
                                [ [ Just S, Nothing, Nothing ]
                                , [ Just S, Just S, Nothing ]
                                , [ Nothing, Just S, Nothing ]
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
        ]
