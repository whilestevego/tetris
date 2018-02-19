module Game.Grid
    exposing
        ( Grid
        , Coordinate
        , columns
        , coordinateMap
        , fromList
        , get
        , initialize
        , map
        , rotate90
        , rows
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
    ( Column, Row )


coordinateGetter : Int -> Int -> Coordinate
coordinateGetter columns index =
    ( index % columns, index // columns )


columns : Grid a -> Int
columns grid =
    grid.columns


rows : Grid a -> Int
rows { columns, data } =
    data |> Array.length |> flip (//) columns


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


rotate90 : Grid a -> Grid a
rotate90 grid =
    let
        rs =
            rows grid

        cs =
            columns grid
    in
        initialize
            rs
            cs
            (\( col, row ) -> getUnsafe ( row, rs - 1 - col ) grid)


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
            Array.get (col + row * columns) data


getUnsafe : Coordinate -> Grid a -> a
getUnsafe coords grid =
    case get coords grid of
        Just x ->
            x

        Nothing ->
            Debug.crash
                ("Coordinates "
                    ++ toString coords
                    ++ " of Grid are not reachable."
                )


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
