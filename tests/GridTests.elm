module GridTests exposing (..)

import Test exposing (..)
import Expect
import Grid exposing (..)
import Array


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Grid"
        [ test "initialize" <|
            \_ ->
                Expect.equal
                    (initialize 2 2 identity)
                    (Grid
                        2
                        (Array.fromList [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ])
                    )
        , test "toList" <|
            \_ ->
                Expect.equal
                    (toList (initialize 2 2 identity))
                    ([ [ ( 0, 0 ), ( 0, 1 ) ], [ ( 1, 0 ), ( 1, 1 ) ] ])
        , test "get within grid" <|
            \_ ->
                Expect.equal
                    (get ( 1, 1 ) (initialize 2 2 identity))
                    (Just ( 1, 1 ))
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
        , test "set within grid" <|
            \_ ->
                Expect.equal
                    (set ( 0, 0 ) "Hello" (initialize 2 2 (always "Bye")))
                    (Grid
                        2
                        (Array.fromList [ "Hello", "Bye", "Bye", "Bye" ])
                    )
        ]
