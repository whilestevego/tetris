module Game.Tetromino exposing (..)

import Game.Grid as Grid exposing (Grid)


type TetrominoBlock
    = I
    | O
    | T
    | J
    | L
    | S
    | Z


type alias Tetromino =
    Grid (Maybe TetrominoBlock)


tetrominoI : Tetromino
tetrominoI =
    Grid.fromList
        [ [ Nothing, Nothing, Nothing, Nothing ]
        , [ Just I, Just I, Just I, Just I ]
        , [ Nothing, Nothing, Nothing, Nothing ]
        , [ Nothing, Nothing, Nothing, Nothing ]
        ]


tetrominoO : Tetromino
tetrominoO =
    Grid.fromList
        [ [ Just O, Just O ]
        , [ Just O, Just O ]
        ]


tetrominoT : Tetromino
tetrominoT =
    Grid.fromList
        [ [ Nothing, Nothing, Nothing ]
        , [ Nothing, Just T, Nothing ]
        , [ Just T, Just T, Just T ]
        ]


tetrominoS : Tetromino
tetrominoS =
    Grid.fromList
        [ [ Nothing, Nothing, Nothing ]
        , [ Nothing, Just S, Just S ]
        , [ Just S, Just S, Nothing ]
        ]


tetrominoZ : Tetromino
tetrominoZ =
    Grid.fromList
        [ [ Nothing, Nothing, Nothing ]
        , [ Just Z, Just Z, Nothing ]
        , [ Nothing, Just Z, Just Z ]
        ]


tetrominoJ : Tetromino
tetrominoJ =
    Grid.fromList
        [ [ Nothing, Nothing, Nothing ]
        , [ Just J, Nothing, Nothing ]
        , [ Just J, Just J, Just J ]
        ]


tetrominoL : Tetromino
tetrominoL =
    Grid.fromList
        [ [ Nothing, Nothing, Nothing ]
        , [ Nothing, Nothing, Just L ]
        , [ Just L, Just L, Just L ]
        ]
