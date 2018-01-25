module Grid
    exposing
        ( Grid
        , coordinateMap
        , fromList
        , get
        , initialize
        , map
        , set
        , toList
        )

import List.Extra exposing (groupsOf)
import Array exposing (Array)


type alias Grid a =
    { columns : Int
    , data : Array a
    }


type alias Row =
    Int


type alias Column =
    Int


type alias Coordinate =
    ( Row, Column )


coordinateGetter : Int -> Int -> Coordinate
coordinateGetter columns index =
    ( index // columns, index % columns )


fromList : List (List a) -> Grid a
fromList list =
    let
        columns =
            list |> List.head |> Maybe.withDefault [] |> List.length
    in
        Grid
            columns
            (list
                |> List.concatMap (List.take columns)
                |> Array.fromList
            )


initialize : Int -> Int -> (Coordinate -> a) -> Grid a
initialize columns rows f =
    let
        coords =
            coordinateGetter columns
    in
        Grid
            columns
            (Array.initialize (columns * rows) (\i -> f (coords i)))


toList : Grid a -> List (List a)
toList { columns, data } =
    data
        |> Array.toList
        |> groupsOf columns


get : Coordinate -> Grid a -> Maybe a
get ( col, row ) { data, columns } =
    let
        rows =
            (Array.length data) // columns
    in
        if (col >= columns || row >= rows) then
            Nothing
        else
            Array.get (col * columns + row) data


set : Coordinate -> a -> Grid a -> Grid a
set ( col, row ) value grid =
    let
        { columns, data } =
            grid

        rows =
            (Array.length data) // columns
    in
        if (col >= columns || row >= rows) then
            grid
        else
            Grid columns (Array.set (col * columns + row) value data)


coordinateMap : (Coordinate -> a -> b) -> Grid a -> Grid b
coordinateMap f { columns, data } =
    let
        coords =
            coordinateGetter columns
    in
        data
            |> Array.indexedMap (\i v -> f (coords i) v)
            |> Grid columns


map : (a -> b) -> Grid a -> Grid b
map f { columns, data } =
    data
        |> Array.map f
        |> Grid columns
