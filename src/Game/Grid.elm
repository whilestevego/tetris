module Game.Grid
    exposing
        ( Grid
        , Coordinate
        , columnLength
        , coordinateFoldl
        , coordinateMap
        , findCoordinate
        , findCoordinateReverse
        , fromList
        , get
        , isNothing
        , initialize
        , map
        , mergeAt
        , rotate180
        , rotate270
        , rotate90
        , rowLength
        , set
        , toFlatList
        , toList
        )

import List.Extra exposing (groupsOf)
import Array exposing (Array)


-- TODO: Move to an Array module


findIndex : (a -> Bool) -> Array a -> Maybe Int
findIndex pred arr =
    arr
        |> Array.toList
        |> List.Extra.findIndex pred


findIndexReverse : (a -> Bool) -> Array a -> Maybe Int
findIndexReverse pred arr =
    arr
        |> Array.toList
        |> List.reverse
        |> List.Extra.findIndex pred
        |> Maybe.map (\x -> Array.length arr - 1 - x)


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


coordinatesFromCols : Int -> Int -> Coordinate
coordinatesFromCols columns index =
    ( index % columns, index // columns )


columnLength : Grid a -> Int
columnLength grid =
    grid.columns


rowLength : Grid a -> Int
rowLength { columns, data } =
    data |> Array.length |> flip (//) columns


fromList : List (List a) -> Grid a
fromList list =
    let
        columns =
            list |> List.head |> Maybe.withDefault [] |> List.length

        data =
            list
                |> List.concatMap (List.take columns)
                |> Array.fromList
    in
        Grid columns data


rotate90 : Grid a -> Grid a
rotate90 grid =
    let
        columns =
            columnLength grid

        rows =
            rowLength grid
    in
        initialize
            rows
            columns
            (\( x, y ) -> getUnsafe ( y, rows - 1 - x ) grid)


rotate180 : Grid a -> Grid a
rotate180 grid =
    let
        columns =
            columnLength grid

        rows =
            rowLength grid
    in
        initialize
            columns
            rows
            (\( x, y ) -> getUnsafe ( columns - 1 - x, rows - 1 - y ) grid)


rotate270 : Grid a -> Grid a
rotate270 grid =
    let
        columns =
            columnLength grid

        rows =
            rowLength grid
    in
        initialize
            rows
            columns
            (\( x, y ) -> getUnsafe ( columns - 1 - y, x ) grid)


initialize : Int -> Int -> (Coordinate -> a) -> Grid a
initialize columns rows f =
    let
        coords =
            coordinatesFromCols columns
    in
        Grid
            columns
            (Array.initialize (columns * rows) (\i -> f (coords i)))


toList : Grid a -> List (List a)
toList { columns, data } =
    data
        |> Array.toList
        |> groupsOf columns


toFlatList : Grid a -> List a
toFlatList { columns, data } =
    data
        |> Array.toList


findCoordinate : (a -> Bool) -> Grid a -> Maybe Coordinate
findCoordinate pred { columns, data } =
    data
        |> findIndex pred
        |> Maybe.map (coordinatesFromCols columns)


findCoordinateReverse : (a -> Bool) -> Grid a -> Maybe Coordinate
findCoordinateReverse pred { columns, data } =
    data
        |> findIndexReverse pred
        |> Maybe.map (coordinatesFromCols columns)


isNothing : Coordinate -> Grid (Maybe a) -> Bool
isNothing pos grid =
    case get pos grid of
        Just v ->
            case v of
                Just _ ->
                    False

                Nothing ->
                    True

        Nothing ->
            False


get : Coordinate -> Grid a -> Maybe a
get ( x, y ) { data, columns } =
    let
        rows =
            (Array.length data) // columns
    in
        if (x >= columns || y >= rows) then
            Nothing
        else
            Array.get (x + y * columns) data


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
set ( x, y ) value grid =
    let
        { columns, data } =
            grid

        rows =
            (Array.length data) // columns
    in
        if (x >= columns || y >= rows) then
            grid
        else
            Grid columns (Array.set (x + y * columns) value data)


coordinateFoldl : (Coordinate -> a -> b -> b) -> b -> Grid a -> b
coordinateFoldl fn init grid =
    let
        { columns, data } =
            grid
    in
        data
            |> Array.toIndexedList
            |> List.foldl
                (\( index, value ) acc ->
                    fn
                        (coordinatesFromCols columns index)
                        value
                        acc
                )
                init


coordinateMap : (Coordinate -> a -> b) -> Grid a -> Grid b
coordinateMap f { columns, data } =
    data
        |> Array.indexedMap (\i v -> f (coordinatesFromCols columns i) v)
        |> Grid columns


map : (a -> b) -> Grid a -> Grid b
map f { columns, data } =
    data
        |> Array.map f
        |> Grid columns


mergeAt : Grid a -> Coordinate -> Grid a -> Grid a
mergeAt gridA ( baseX, baseY ) gridB =
    gridA
        |> coordinateMap (\coords value -> ( coords, value ))
        |> toFlatList
        |> List.foldl
            (\( ( x, y ), value ) grid ->
                set ( baseX + x, baseY + y ) value grid
            )
            gridB
