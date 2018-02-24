module Game.Tetromino exposing (..)

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
    }


create : Block -> Coordinate -> Tetromino
create blockType pos =
    Tetromino (generateBlocks blockType) pos


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
