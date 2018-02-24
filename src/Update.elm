module Update exposing (..)

import Game.Tetromino as Tetromino
import Model exposing (..)
import Time exposing (Time)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationDiffs dt ->
            ( updateTime dt model, Cmd.none )


updateTime : Float -> Model -> Model
updateTime dt model =
    let
        newDt =
            model.dt + dt

        newTick =
            newDt |> Time.inSeconds |> truncate

        mapper =
            if (newTick > model.tick) then
                gameTick
            else
                identity
    in
        mapper
            { model
                | dt = newDt
                , tick = newTick
            }


(>=>) : Maybe a -> (a -> b) -> Maybe b
(>=>) =
    flip Maybe.map
infixr 9 >=>


gameTick : Model -> Model
gameTick model =
    let
        { tick, activeTetromino, board } =
            model

        newActiveTetromino =
            case activeTetromino of
                Just tetro ->
                    if (Tuple.second tetro.position) > 21 then
                        activeTetromino >=> Tetromino.setPos ( 0, -1 )
                    else
                        activeTetromino >=> Tetromino.moveDown

                Nothing ->
                    activeTetromino
    in
        { model
            | activeTetromino = newActiveTetromino
        }
