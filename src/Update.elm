module Update exposing (..)

import Game.Grid as Grid exposing (Grid)
import Game.Tetromino as T exposing (Tetromino, Block(..))
import Keyboard exposing (KeyCode)
import Model exposing (..)
import Random
import Set exposing (Set)
import Stuff.Keyboard exposing (..)
import Time exposing (Time)


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

        KeyDown keyCode ->
            model
                |> saveInput keyCode
                |> applyCommands
                |> flip (,) Cmd.none

        KeyUp keyCode ->
            model
                |> removeInput keyCode
                |> flip (,) Cmd.none


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



-- KEYBOARD INPUT EVENTS --


saveInput : KeyCode -> Model -> Model
saveInput keyCode model =
    -- Modifers are Shift | Control | Alt | Meta
    if modifiers |> Set.member keyCode then
        { model
            | inputMods = model.inputMods |> Set.insert keyCode
        }
    else
        { model
            | inputKey = Just keyCode
        }


removeInput : KeyCode -> Model -> Model
removeInput keyCode model =
    if modifiers |> Set.member keyCode then
        { model
            | inputMods = model.inputMods |> Set.remove keyCode
        }
    else
        { model
            | inputKey = Nothing
        }


applyCommands : Model -> Model
applyCommands model =
    let
        { inputKey, inputMods, activeTetromino } =
            model

        _ =
            Debug.log "input" ( inputKey, inputMods )
    in
        case inputKey of
            Just key ->
                case key of
                    -- Right Arrow
                    39 ->
                        applyMove T.moveRight model

                    -- Left Arrow
                    37 ->
                        applyMove T.moveLeft model

                    -- Z
                    90 ->
                        applyMove T.rotateRight model

                    -- X
                    88 ->
                        applyMove T.rotateLeft model

                    _ ->
                        model

            Nothing ->
                model


applyMove : (Tetromino -> Tetromino) -> Model -> Model
applyMove moveFn model =
    let
        { activeTetromino, board } =
            model
    in
        case activeTetromino of
            Nothing ->
                model

            Just tetro ->
                if detectCollision (tetro |> moveFn) board then
                    model
                else
                    { model
                        | activeTetromino = tetro |> moveFn |> Just
                    }



-- GAME TICK EVENTS --


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
                        , board = tetro |> T.mergeWith board
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
