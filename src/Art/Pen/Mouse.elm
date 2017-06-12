module Art.Pen.Mouse exposing (Msg, State, update, subs, newState)

import Task
import Mouse
import Art.Pen as Pen


type Msg
    = DrawAt Mouse.Position
    | LiftMouse


type alias State =
    Msg


newState : State
newState =
    LiftMouse


update : Msg -> State -> ( State, Cmd Pen.Msg )
update curstate oldstate =
    let
        run task =
            Task.perform identity (Task.succeed task)

        ret task =
            ( curstate, run task )
    in
        case curstate of
            DrawAt mousepos ->
                let
                    mousepos_ =
                        { x = toFloat mousepos.x, y = toFloat mousepos.y }
                in
                    case oldstate of
                        DrawAt _ ->
                            ret (Pen.DrawAbsolute mousepos_)

                        -- Update the canvas position and then draw.
                        LiftMouse ->
                            ( curstate
                            , Cmd.batch
                                [ run Pen.UpdatePosition
                                , run (Pen.DrawAbsolute mousepos_)
                                ]
                            )

            LiftMouse ->
                ret Pen.Lift


subs : State -> Sub Msg
subs state =
    let
        movementSub =
            case state of
                -- NOTE: if canvas move while drawing, introduces offset
                DrawAt _ ->
                    Mouse.moves DrawAt

                LiftMouse ->
                    Sub.none
    in
        Sub.batch
            [ movementSub
            , Mouse.ups (\x -> LiftMouse)
            , Mouse.downs DrawAt
            ]
