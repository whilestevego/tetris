module Game.Grid
    exposing
        ( Grid
        , Coordinate
        , Column
        , Row
        , columnLength
        , coordinateFoldl
        , coordinateMap
        , coordinateAll
        , findCoordinate
        , findCoordinateReverse
        , fromList
        , get
        , isSomething
        , initialize
        , isNothing
        , map
        , mergeAt
        , rotate180
        , rotate270
        , rotate90
        , rowLength
        , set
        , slice
        , getRow
        , toFlatList
        , toList
        )

import List.Extra exposing (groupsOf)
import Array exposing (Array)


findIndex : (a -> Bool) -> Array a -> Maybe Int
findIndex pred arr =
    -- TODO: Move to an Array module
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



-- PUBLIC --


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
    -- TODO: Add tests for negative values of columns and rows
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


isSomething : Coordinate -> Grid (Maybe a) -> Bool
isSomething pos grid =
    case get pos grid of
        Just v ->
            case v of
                Just _ ->
                    True

                Nothing ->
                    False

        Nothing ->
            False


get : Coordinate -> Grid a -> Maybe a
get ( x, y ) { data, columns } =
    let
        rows =
            (Array.length data) // columns
    in
        if (x < 0 || x >= columns || y < 0 || y >= rows) then
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


slice : Coordinate -> Coordinate -> Grid a -> Grid a
slice ( aX, aY ) ( bX, bY ) grid =
    let
        -- Start Coordinates
        ( sX, sY ) =
            ( min aX bX, min aY bY ) |> clampInside grid

        -- End Coordinates
        ( eX, eY ) =
            ( max aX bX, max aY bY ) |> clampInside grid

        columns =
            (eX - sX)

        rows =
            (eY - sY)
    in
        initialize
            (if rows < 1 then
                0
             else
                columns
            )
            (if columns < 1 then
                0
             else
                rows
            )
            (\( x, y ) -> getUnsafe ( sX + x, sY + y ) grid)


getRow : Row -> Grid a -> Grid a
getRow row grid =
    grid
        |> slice ( 0, row ) ( grid |> columnLength, row + 1 )


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


coordinateAll : (Coordinate -> a -> Bool) -> Grid a -> Bool
coordinateAll f grid =
    grid
        |> coordinateFoldl
            (\coord block acc -> f coord block && acc)
            True


map : (a -> b) -> Grid a -> Grid b
map f { columns, data } =
    data
        |> Array.map f
        |> Grid columns


mergeAt : Grid a -> Coordinate -> Grid a -> Grid a
mergeAt gridA ( oX, oY ) gridB =
    gridA
        |> coordinateFoldl
            (\( x, y ) value grid ->
                set ( oX + x, oY + y ) value grid
            )
            gridB



-- PRIVATE --


clampInside : Grid a -> Coordinate -> Coordinate
clampInside grid ( x, y ) =
    ( clamp 0 (grid |> columnLength) x
    , clamp 0 (grid |> rowLength) y
    )


coordinatesFromCols : Int -> Int -> Coordinate
coordinatesFromCols columns index =
    ( index % columns, index // columns )
