module Art.Pen.Mouse exposing (newInput, State, Msg)

import Mouse
import Art.Canvas as Canvas


type Msg
    = DrawAt Mouse.Position
    | LiftMouse


type alias State =
    Msg


update : Msg -> State -> ( State, Cmd Canvas.Msg )
update msg _ =
    ( msg
    , case msg of
        DrawAt { x, y } ->
            Canvas.drawAbsolute { x = toFloat x, y = toFloat y }

        LiftMouse ->
            Canvas.lift
    )


subs : State -> Sub Msg
subs state =
    let
        movementSub =
            case state of
                DrawAt _ ->
                    Mouse.moves DrawAt

                LiftMouse ->
                    Sub.none
    in
        Sub.batch
            [ movementSub
            , Mouse.ups <| always LiftMouse
            , Mouse.downs DrawAt
            ]


newInput : Canvas.Input State Msg
newInput =
    { state = LiftMouse
    , update = update
    , subs = subs
    }
