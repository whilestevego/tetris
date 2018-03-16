module Game.Tetromino
    exposing
        ( Tetromino
        , Block(..)
        , coordinateFoldl
        , create
        , rotateRight
        , rotateLeft
        , setPos
        , moveDown
        , moveLeft
        , moveRight
        , mergeWith
        , getX
        , getY
        , getTopBound
        , getRightBound
        , getBottomBound
        , getLeftBound
        , getOccupiedColumns
        , getOccupiedRows
        , toPositionList
        , toPositionSet
        , typeFromInt
        , color
        )

import Game.Grid as Grid exposing (Grid, Coordinate, Column, Row)
import Set exposing (Set)


type Block
    = I
    | O
    | T
    | J
    | L
    | S
    | Z


type alias Tetromino =
    { blocks : Grid (Maybe Block)
    , position : Coordinate
    , bounds : ( Coordinate, Coordinate )
    }


create : Block -> Coordinate -> Tetromino
create blockType pos =
    let
        blocks =
            (generateBlocks blockType)

        bounds =
            (calculateBounds blocks)
    in
        Tetromino blocks pos bounds


generateBlocks : Block -> Grid (Maybe Block)
generateBlocks blockType =
    case blockType of
        I ->
            Grid.fromList
                [ [ Nothing, Nothing, Nothing, Nothing ]
                , [ Just I, Just I, Just I, Just I ]
                , [ Nothing, Nothing, Nothing, Nothing ]
                , [ Nothing, Nothing, Nothing, Nothing ]
                ]

        O ->
            Grid.fromList
                [ [ Just O, Just O ]
                , [ Just O, Just O ]
                ]

        T ->
            Grid.fromList
                [ [ Nothing, Just T, Nothing ]
                , [ Just T, Just T, Just T ]
                , [ Nothing, Nothing, Nothing ]
                ]

        J ->
            Grid.fromList
                [ [ Just J, Nothing, Nothing ]
                , [ Just J, Just J, Just J ]
                , [ Nothing, Nothing, Nothing ]
                ]

        L ->
            Grid.fromList
                [ [ Nothing, Nothing, Just L ]
                , [ Just L, Just L, Just L ]
                , [ Nothing, Nothing, Nothing ]
                ]

        S ->
            Grid.fromList
                [ [ Nothing, Nothing, Nothing ]
                , [ Nothing, Just S, Just S ]
                , [ Just S, Just S, Nothing ]
                ]

        Z ->
            Grid.fromList
                [ [ Nothing, Nothing, Nothing ]
                , [ Just Z, Just Z, Nothing ]
                , [ Nothing, Just Z, Just Z ]
                ]


firstBlockPos : Grid (Maybe Block) -> Coordinate
firstBlockPos blocks =
    case Grid.findCoordinate ((/=) Nothing) blocks of
        Just pos ->
            pos

        Nothing ->
            Debug.crash "Could not find block in Tetromino data"


lastBlockPos : Grid (Maybe Block) -> Coordinate
lastBlockPos blocks =
    case Grid.findCoordinateReverse ((/=) Nothing) blocks of
        Just pos ->
            pos

        Nothing ->
            Debug.crash "Could not find block in Tetromino data"


calculateBounds : Grid (Maybe Block) -> ( Coordinate, Coordinate )
calculateBounds blocks =
    let
        maybeBounds =
            blocks
                |> Grid.coordinateFoldl
                    (\( x, y ) block bounds ->
                        case block of
                            Just _ ->
                                case bounds of
                                    Nothing ->
                                        Just ( ( x, y ), ( x, y ) )

                                    Just bounds ->
                                        let
                                            ( ( uLX, uLY ), ( lRX, lRY ) ) =
                                                bounds
                                        in
                                            Just
                                                ( ( min x uLX, min y uLY )
                                                , ( max x lRX, max y lRY )
                                                )

                            Nothing ->
                                bounds
                    )
                    Nothing
    in
        case maybeBounds of
            Just bounds ->
                bounds

            Nothing ->
                Debug.crash "Could not find any block within grid"



-- BOUNDS OPS --


getBounds : Tetromino -> ( Coordinate, Coordinate )
getBounds { position, bounds } =
    let
        ( pX, pY ) =
            position

        ( ( tLX, tLY ), ( bRX, bRY ) ) =
            bounds
    in
        ( ( pX + tLX, pY + tLY ), ( pX + bRX, pY + bRY ) )


getTopBound : Tetromino -> Int
getTopBound tetromino =
    tetromino |> getBounds |> Tuple.first |> Tuple.second


getRightBound : Tetromino -> Int
getRightBound tetromino =
    tetromino |> getBounds |> Tuple.second |> Tuple.first


getBottomBound : Tetromino -> Int
getBottomBound tetromino =
    tetromino |> getBounds |> Tuple.second |> Tuple.second


getLeftBound : Tetromino -> Int
getLeftBound tetromino =
    tetromino |> getBounds |> Tuple.first |> Tuple.first


rotateLeft : Tetromino -> Tetromino
rotateLeft tetromino =
    let
        blocks =
            Grid.rotate270 tetromino.blocks

        bounds =
            calculateBounds blocks
    in
        { tetromino
            | blocks = blocks
            , bounds = bounds
        }


rotateRight : Tetromino -> Tetromino
rotateRight tetromino =
    let
        blocks =
            Grid.rotate90 tetromino.blocks

        bounds =
            calculateBounds blocks
    in
        { tetromino
            | blocks = blocks
            , bounds = bounds
        }



-- POSITION OPS --


setPos : Coordinate -> Tetromino -> Tetromino
setPos pos tetromino =
    { tetromino | position = pos }


moveDown : Tetromino -> Tetromino
moveDown tetromino =
    let
        ( pX, pY ) =
            tetromino.position
    in
        setPos ( pX, pY + 1 ) tetromino


moveLeft : Tetromino -> Tetromino
moveLeft tetromino =
    let
        ( pX, pY ) =
            tetromino.position
    in
        setPos ( pX - 1, pY ) tetromino


moveRight : Tetromino -> Tetromino
moveRight tetromino =
    let
        ( pX, pY ) =
            tetromino.position
    in
        setPos ( pX + 1, pY ) tetromino


getX : Tetromino -> Column
getX tetromino =
    Tuple.first tetromino.position


getY : Tetromino -> Row
getY tetromino =
    Tuple.second tetromino.position


getOccupiedRows : Tetromino -> Set Row
getOccupiedRows tetromino =
    tetromino
        |> toPositionSet
        |> Set.map (Tuple.second)


getOccupiedColumns : Tetromino -> Set Column
getOccupiedColumns tetromino =
    tetromino
        |> toPositionSet
        |> Set.map (Tuple.first)


coordinateFoldl : (Coordinate -> Maybe Block -> b -> b) -> b -> Tetromino -> b
coordinateFoldl fn init { blocks, position } =
    let
        ( pX, pY ) =
            position
    in
        Grid.coordinateFoldl
            (\( x, y ) block acc -> fn ( x + pX, y + pY ) block acc)
            init
            blocks


toPositionSet : Tetromino -> Set Coordinate
toPositionSet tetromino =
    tetromino
        |> coordinateFoldl
            (\pos block acc ->
                case block of
                    Just _ ->
                        acc |> Set.insert pos

                    Nothing ->
                        acc
            )
            Set.empty


toPositionList : Tetromino -> List Coordinate
toPositionList tetromino =
    coordinateFoldl
        (\pos block acc ->
            case block of
                Just _ ->
                    pos :: acc

                Nothing ->
                    acc
        )
        []
        tetromino


mergeWith : Grid (Maybe Block) -> Tetromino -> Grid (Maybe Block)
mergeWith board tetro =
    tetro
        |> coordinateFoldl
            (\pos mBlock acc ->
                case mBlock of
                    Just block ->
                        Grid.set pos (Just block) acc

                    Nothing ->
                        acc
            )
            board



-- MISC --


typeFromInt : Int -> Block
typeFromInt x =
    case x of
        1 ->
            O

        2 ->
            T

        3 ->
            J

        4 ->
            L

        5 ->
            S

        6 ->
            Z

        _ ->
            I


color : Block -> String
color blockType =
    case blockType of
        I ->
            "#E91E63"

        O ->
            "#673AB7"

        T ->
            "#2196F3"

        J ->
            "#00BCD4"

        L ->
            "#4CAF50"

        S ->
            "#CDDC39"

        Z ->
            "#FFC107"
