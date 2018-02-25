module Game.Tetromino
    exposing
        ( Tetromino
        , Block(..)
        , create
        , rotateRight
        , setPos
        , moveDown
        , getX
        , getY
        , color
        )

import Game.Grid as Grid exposing (Grid, Coordinate)


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
                [ [ Nothing, Nothing, Nothing ]
                , [ Nothing, Just T, Nothing ]
                , [ Just T, Just T, Just T ]
                ]

        J ->
            Grid.fromList
                [ [ Nothing, Nothing, Nothing ]
                , [ Nothing, Just S, Just S ]
                , [ Just S, Just S, Nothing ]
                ]

        L ->
            Grid.fromList
                [ [ Nothing, Nothing, Nothing ]
                , [ Just Z, Just Z, Nothing ]
                , [ Nothing, Just Z, Just Z ]
                ]

        S ->
            Grid.fromList
                [ [ Nothing, Nothing, Nothing ]
                , [ Just J, Nothing, Nothing ]
                , [ Just J, Just J, Just J ]
                ]

        Z ->
            Grid.fromList
                [ [ Nothing, Nothing, Nothing ]
                , [ Nothing, Nothing, Just L ]
                , [ Just L, Just L, Just L ]
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

        ( ( uLX, uLY ), ( lRX, lRY ) ) =
            bounds
    in
        ( ( pX + uLX, pY + uLY ), ( pX + lRX, pY + lRY ) )


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


getX : Tetromino -> Int
getX tetromino =
    Tuple.first tetromino.position


getY : Tetromino -> Int
getY tetromino =
    Tuple.second tetromino.position



-- MISC --


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
