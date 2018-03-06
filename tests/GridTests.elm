module GridTests exposing (..)

import Test exposing (..)
import Expect
import Game.Grid as Grid exposing (..)
import Array


all : Test
all =
    describe "Grid"
        [ describe "mergeAt"
            [ test "( 0, 0 ) in bounds" <|
                \_ ->
                    let
                        guest =
                            fromList
                                [ [ 1, 2 ]
                                , [ 3, 4 ]
                                ]

                        host =
                            fromList
                                [ [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                ]
                    in
                        Expect.equal
                            (fromList
                                [ [ 1, 2, 0, 0 ]
                                , [ 3, 4, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                ]
                            )
                            (mergeAt guest ( 0, 0 ) host)
            , test "( 2, 3 ) in bounds" <|
                \_ ->
                    let
                        guest =
                            fromList
                                [ [ 1, 2 ]
                                , [ 3, 4 ]
                                ]

                        host =
                            fromList
                                [ [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                ]
                    in
                        Expect.equal
                            (fromList
                                [ [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 1, 2 ]
                                , [ 0, 0, 3, 4 ]
                                ]
                            )
                            (mergeAt guest ( 2, 3 ) host)
            , test "( 3, 1 ) at bounds" <|
                \_ ->
                    let
                        guest =
                            fromList
                                [ [ 1, 2 ]
                                , [ 3, 4 ]
                                ]

                        host =
                            fromList
                                [ [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                ]
                    in
                        Expect.equal
                            (fromList
                                [ [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 1 ]
                                , [ 0, 0, 0, 3 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                ]
                            )
                            (mergeAt guest ( 3, 1 ) host)
            , test "( -1, -1 ) outside bounds" <|
                \_ ->
                    let
                        guest =
                            fromList
                                [ [ 1, 2 ]
                                , [ 3, 4 ]
                                ]

                        host =
                            fromList
                                [ [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                ]
                    in
                        Expect.equal
                            (fromList
                                [ [ 4, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0 ]
                                ]
                            )
                            (mergeAt guest ( -1, -1 ) host)
            ]
        , test "fromList" <|
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
        , describe "findCoordinate"
            [ test "with something to find" <|
                \_ ->
                    let
                        grid =
                            fromList [ [ "Bye", "Yo" ], [ "Yo", "Bye" ] ]
                    in
                        Expect.equal
                            (findCoordinate ((==) "Yo") grid)
                            (Just ( 1, 0 ))
            , test "with nothing to find" <|
                \_ ->
                    let
                        grid =
                            fromList [ [ "Bye", "Bye" ], [ "Yo", "Bye" ] ]
                    in
                        Expect.equal
                            (findCoordinate ((==) "Hallo") grid)
                            Nothing
            ]
        , describe "findIndexReverse"
            [ test "with something to find" <|
                \_ ->
                    let
                        grid =
                            fromList [ [ "Bye", "Yo" ], [ "Yo", "Bye" ] ]
                    in
                        Expect.equal
                            (findCoordinateReverse ((==) "Yo") grid)
                            (Just ( 0, 1 ))
            , test "with nothing to find" <|
                \_ ->
                    let
                        grid =
                            fromList [ [ "Bye", "Bye" ], [ "Yo", "Bye" ] ]
                    in
                        Expect.equal
                            (findCoordinate ((==) "Hallo") grid)
                            Nothing
            ]
        , describe "get"
            [ test "within square grid" <|
                \_ ->
                    Expect.equal
                        (get ( 1, 1 ) (initialize 2 2 identity))
                        (Just ( 1, 1 ))
            , test "within rectangular grid" <|
                \_ ->
                    Expect.equal
                        (get ( 0, 4 ) (initialize 3 5 identity))
                        (Just ( 0, 4 ))
            , test "outside col left-side" <|
                \_ ->
                    Expect.equal
                        (get ( -1, 2 ) (initialize 3 3 identity))
                        Nothing
            , test "outside col right-side" <|
                \_ ->
                    Expect.equal
                        (get ( 3, 2 ) (initialize 3 3 identity))
                        Nothing
            , test "outside row bottom-side" <|
                \_ ->
                    Expect.equal
                        (get ( 2, 3 ) (initialize 3 3 identity))
                        Nothing
            , test "outside row top-side" <|
                \_ ->
                    Expect.equal
                        (get ( 2, -1 ) (initialize 3 3 identity))
                        Nothing
            ]
        , describe "set"
            [ test "first within grid" <|
                \_ ->
                    let
                        grid =
                            fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                    in
                        Expect.equal
                            (set ( 0, 0 ) "Hello" grid)
                            (fromList [ [ "Hello", "Bye" ], [ "Bye", "Bye" ] ])
            , test "second within grid" <|
                \_ ->
                    let
                        grid =
                            fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                    in
                        Expect.equal
                            (set ( 1, 0 ) "Hello" grid)
                            (fromList [ [ "Bye", "Hello" ], [ "Bye", "Bye" ] ])
            , test "third within grid" <|
                \_ ->
                    let
                        grid =
                            fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                    in
                        Expect.equal
                            (set ( 0, 1 ) "Hello" grid)
                            (fromList [ [ "Bye", "Bye" ], [ "Hello", "Bye" ] ])
            , test "last within grid" <|
                \_ ->
                    let
                        grid =
                            fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                    in
                        Expect.equal
                            (set ( 1, 1 ) "Hello" grid)
                            (fromList [ [ "Bye", "Bye" ], [ "Bye", "Hello" ] ])
            , test "outside col" <|
                \_ ->
                    let
                        grid =
                            fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                    in
                        Expect.equal
                            (set ( 2, 0 ) "Hello" grid)
                            grid
            , test "outside row" <|
                \_ ->
                    let
                        grid =
                            fromList [ [ "Bye", "Bye" ], [ "Bye", "Bye" ] ]
                    in
                        Expect.equal
                            (set ( 0, 2 ) "Hello" grid)
                            grid
            ]
        , describe "slice"
            [ test "( 0, 0 ) to ( 0, 0 )" <|
                \_ ->
                    let
                        grid =
                            fromList
                                [ [ "Bye", "Hi", "Bye" ]
                                , [ "Hi", "Yo", "Hi" ]
                                , [ "Bye", "Hi", "Bye" ]
                                ]
                    in
                        Expect.equal
                            (grid |> slice ( 0, 0 ) ( 0, 0 ))
                            (fromList [])
            , test "slicing a square from ( 0, 0 ) to ( 2, 2)" <|
                \_ ->
                    let
                        grid =
                            fromList
                                [ [ "Bye", "Hi", "Bye" ]
                                , [ "Hi", "Yo", "Hi" ]
                                , [ "Bye", "Hi", "Bye" ]
                                ]
                    in
                        Expect.equal
                            (grid |> slice ( 0, 0 ) ( 2, 2 ))
                            (fromList
                                [ [ "Bye", "Hi" ]
                                , [ "Hi", "Yo" ]
                                ]
                            )
            , test "slicing column 1 using ( 1, 0 ) to ( 2, 3 )" <|
                \_ ->
                    let
                        grid =
                            fromList
                                [ [ "Bye", "Hi", "Bye" ]
                                , [ "Hi", "Yo", "Hi" ]
                                , [ "Bye", "Hi", "Bye" ]
                                ]
                    in
                        Expect.equal
                            (grid |> slice ( 1, 0 ) ( 2, 3 ))
                            (fromList
                                [ [ "Hi" ]
                                , [ "Yo" ]
                                , [ "Hi" ]
                                ]
                            )
            , test "slicing row 1 using ( 0, 1 ) to ( 3, 2 )" <|
                \_ ->
                    let
                        grid =
                            fromList
                                [ [ "Bye", "Hi", "Bye" ]
                                , [ "Hi", "Yo", "Hi" ]
                                , [ "Bye", "Hi", "Bye" ]
                                ]
                    in
                        Expect.equal
                            (grid |> slice ( 0, 1 ) ( 3, 2 ))
                            (fromList
                                [ [ "Hi", "Yo", "Hi" ]
                                ]
                            )

            -- TODO: Make this more efficient (don't create new grid)
            , test "slicing larger than bounds returns grid" <|
                \_ ->
                    let
                        grid =
                            fromList
                                [ [ "Bye", "Hi", "Bye" ]
                                , [ "Hi", "Yo", "Hi" ]
                                , [ "Bye", "Hi", "Bye" ]
                                ]
                    in
                        Expect.equal
                            (grid |> slice ( -1, -1 ) ( 4, 4 ))
                            (fromList
                                [ [ "Bye", "Hi", "Bye" ]
                                , [ "Hi", "Yo", "Hi" ]
                                , [ "Bye", "Hi", "Bye" ]
                                ]
                            )
            , test "slicing outside bounds returns empty grid" <|
                \_ ->
                    let
                        grid =
                            fromList
                                [ [ "Bye", "Hi", "Bye" ]
                                , [ "Hi", "Yo", "Hi" ]
                                , [ "Bye", "Hi", "Bye" ]
                                ]
                    in
                        Expect.equal
                            (grid |> slice ( -5, -5 ) ( -1, -1 ))
                            (fromList [])
            , test "slicing row 1 using ( -1, 1 ) to ( 4, 2 )" <|
                \_ ->
                    let
                        grid =
                            fromList
                                [ [ "Bye", "Hi", "Bye" ]
                                , [ "Hi", "Yo", "Hi" ]
                                , [ "Bye", "Hi", "Bye" ]
                                ]
                    in
                        Expect.equal
                            (grid |> slice ( -1, 1 ) ( 5, 2 ))
                            (fromList
                                [ [ "Hi", "Yo", "Hi" ]
                                ]
                            )
            , test "slicing column 1 using ( 1, -2 ) to ( 2, 7 )" <|
                \_ ->
                    let
                        grid =
                            fromList
                                [ [ "Bye", "Hi", "Bye" ]
                                , [ "Hi", "Yo", "Hi" ]
                                , [ "Bye", "Hi", "Bye" ]
                                ]
                    in
                        Expect.equal
                            (grid |> slice ( 1, -2 ) ( 2, 8 ))
                            (fromList
                                [ [ "Hi" ]
                                , [ "Yo" ]
                                , [ "Hi" ]
                                ]
                            )
            ]
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
        , test "rotate180" <|
            \_ ->
                let
                    grid =
                        fromList
                            [ [ 1, 2, 3 ]
                            , [ 4, 5, 6 ]
                            , [ 7, 8, 9 ]
                            ]
                in
                    Expect.equal
                        (grid |> rotate180)
                        (fromList
                            [ [ 9, 8, 7 ]
                            , [ 6, 5, 4 ]
                            , [ 3, 2, 1 ]
                            ]
                        )
        , test "rotate270" <|
            \_ ->
                let
                    grid =
                        fromList
                            [ [ 1, 2, 3 ]
                            , [ 4, 5, 6 ]
                            ]
                in
                    Expect.equal
                        (grid |> rotate270)
                        (fromList
                            [ [ 3, 6 ]
                            , [ 2, 5 ]
                            , [ 1, 4 ]
                            ]
                        )
        , test "rotate270 == rotate90 * 3" <|
            \_ ->
                let
                    grid =
                        fromList
                            [ [ 1, 2, 3 ]
                            , [ 4, 5, 6 ]
                            ]
                in
                    Expect.equal
                        (grid |> rotate270)
                        (grid |> rotate90 |> rotate90 |> rotate90)
        , describe "coordinateFoldl"
            [ test "ignoring coordinates" <|
                \_ ->
                    let
                        grid =
                            fromList
                                [ [ 1, 2 ]
                                , [ 3, 4 ]
                                ]
                    in
                        Expect.equal
                            10
                            (coordinateFoldl (\_ v a -> v + a) 0 grid)
            , test "only coordinates" <|
                \_ ->
                    let
                        grid =
                            fromList
                                [ [ 1, 2 ]
                                , [ 3, 4 ]
                                ]
                    in
                        Expect.equal
                            "11011000"
                            (coordinateFoldl
                                (\( x, y ) _ a -> toString x ++ toString y ++ a)
                                ""
                                grid
                            )
            ]
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
