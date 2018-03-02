module Update exposing (..)

import Game.Tetromino as T exposing (Tetromino, Block(..))
import Model exposing (..)
import Time exposing (Time)
import Game.Grid as Grid exposing (Grid)
import Random


-- Todo: Move to module --


(?>) : Maybe a -> (a -> b) -> Maybe b
(?>) =
    flip Maybe.map
infixr 9 ?>


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationDiffs dt ->
            updateTime dt model

        GetRandom num ->
            ( { model | randomNum = num }, Cmd.none )


updateTime : Float -> Model -> ( Model, Cmd Msg )
updateTime dt model =
    let
        nextDt =
            model.dt + dt

        nextTick =
            nextDt |> Time.inSeconds |> ((*) 3) |> truncate

        nextModel =
            { model
                | dt = nextDt
                , tick = nextTick
            }

        mapper =
            if (nextTick > model.tick) then
                gameTick
            else
                identity
    in
        if (nextTick > model.tick) then
            ( gameTick nextModel
            , Random.generate GetRandom (Random.int 1 7)
            )
        else
            ( nextModel, Cmd.none )


gameTick : Model -> Model
gameTick model =
    model
        |> applyGravity
        |> spawnTetromino


spawnTetromino : Model -> Model
spawnTetromino model =
    case model.activeTetromino of
        Nothing ->
            { model
                | activeTetromino =
                    Just
                        (T.create (T.typeFromInt model.randomNum) ( 4, -1 ))
            }

        Just _ ->
            model


applyGravity : Model -> Model
applyGravity model =
    let
        { tick, activeTetromino, board } =
            model
    in
        case activeTetromino of
            Nothing ->
                model

            Just tetro ->
                if detectCollision (tetro |> T.moveDown) board then
                    { model
                        | activeTetromino = Nothing
                        , board = Grid.mergeAt tetro.blocks tetro.position board
                    }
                else
                    { model
                        | activeTetromino = tetro |> T.moveDown |> Just
                    }


detectCollision : Tetromino -> Grid (Maybe Block) -> Bool
detectCollision tetro board =
    tetro
        |> T.toPositionList
        |> List.any (\pos -> board |> Grid.isNothing pos |> not)
