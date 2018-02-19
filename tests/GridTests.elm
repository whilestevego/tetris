module GridTests exposing (..)

import Test exposing (..)
import Expect
import Game.Grid as Grid exposing (..)
import Array


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Grid"
        [ test "fromList" <|
            \_ ->
                Expect.equal
                    (fromList
                        [ [ "Bye", "Bye", "Bye" ]
                        , [ "Bye", "Bye", "Bye" ]
                        ]
                    )
                    (Grid 3
                        (Array.fromList
                            [ "Bye"
                            , "Bye"
                            , "Bye"
                            , "Bye"
                            , "Bye"
                            , "Bye"
                            ]
                        )
                    )
        , test "initialize" <|
            \_ ->
                Expect.equal
                    (initialize 3 2 identity)
                    (fromList
                        [ [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
                        , [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
                        ]
                    )
        , test "toList" <|
            \_ ->
                let
                    grid =
                        fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                in
                    Expect.equal
                        (toList grid)
                        ([ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ])
        , test "get within square grid" <|
            \_ ->
                Expect.equal
                    (get ( 1, 1 ) (initialize 2 2 identity))
                    (Just ( 1, 1 ))
        , test "get within rectangular grid" <|
            \_ ->
                Expect.equal
                    (get ( 0, 4 ) (initialize 3 5 identity))
                    (Just ( 0, 4 ))
        , test "get outside col" <|
            \_ ->
                Expect.equal
                    (get ( 2, 0 ) (initialize 2 2 identity))
                    Nothing
        , test "get outside row" <|
            \_ ->
                Expect.equal
                    (get ( 0, 2 ) (initialize 2 2 identity))
                    Nothing
        , test "set first within grid" <|
            \_ ->
                let
                    grid =
                        fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                in
                    Expect.equal
                        (set ( 0, 0 ) "Hello" grid)
                        (fromList [ [ "Hello", "Bye" ], [ "Bye", "Bye" ] ])
        , test "set last within grid" <|
            \_ ->
                let
                    grid =
                        fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                in
                    Expect.equal
                        (set ( 1, 1 ) "Hello" grid)
                        (fromList [ [ "Bye", "Bye" ], [ "Bye", "Hello" ] ])
        , test "set outside col" <|
            \_ ->
                let
                    grid =
                        fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                in
                    Expect.equal
                        (set ( 2, 0 ) "Hello" grid)
                        grid
        , test "set outside row" <|
            \_ ->
                let
                    grid =
                        fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                in
                    Expect.equal
                        (set ( 0, 2 ) "Hello" grid)
                        grid
        , test "rotate90" <|
            \_ ->
                let
                    grid =
                        fromList
                            [ [ 1, 2, 3 ]
                            , [ 4, 5, 6 ]
                            ]
                in
                    Expect.equal
                        (rotate90 grid)
                        (fromList
                            [ [ 4, 1 ]
                            , [ 5, 2 ]
                            , [ 6, 3 ]
                            ]
                        )
        , test "rotate90 twice" <|
            \_ ->
                let
                    grid =
                        fromList
                            [ [ 1, 2, 3 ]
                            , [ 4, 5, 6 ]
                            ]
                in
                    Expect.equal
                        (grid |> rotate90 |> rotate90)
                        (fromList
                            [ [ 6, 5, 4 ]
                            , [ 3, 2, 1 ]
                            ]
                        )
        , test "rotate90 thrice" <|
            \_ ->
                let
                    grid =
                        fromList
                            [ [ 1, 2, 3 ]
                            , [ 4, 5, 6 ]
                            ]
                in
                    Expect.equal
                        (grid |> rotate90 |> rotate90 |> rotate90)
                        (fromList
                            [ [ 3, 6 ]
                            , [ 2, 5 ]
                            , [ 1, 4 ]
                            ]
                        )
        , test "coordinateMap" <|
            \_ ->
                let
                    grid =
                        fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                in
                    Expect.equal
                        (coordinateMap
                            (\( c, r ) v ->
                                (toString c)
                                    ++ (toString r)
                                    ++ v
                            )
                            grid
                        )
                        (fromList [ [ "00Bye", "10Bye" ], [ "01Bye", "11Bye" ] ])
        , test "map" <|
            \_ ->
                let
                    grid =
                        fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                in
                    Expect.equal
                        (map ((++) "Hello") grid)
                        (fromList
                            [ [ "HelloBye", "HelloBye" ]
                            , [ "HelloBye", "HelloBye" ]
                            ]
                        )
        ]
