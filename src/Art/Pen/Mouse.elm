module Art.Pen.Mouse exposing (newInput, State, Msg)

import Mouse
import Art.Canvas as Canvas


type Msg
    = Hover Mouse.Position
    | Lift
    | Press Mouse.Position


type State
    = Sleeping
    | Activated


update : Msg -> State -> ( State, Cmd Canvas.Msg )
update msg state =
    case msg of
        Press { x, y } ->
            ( Activated
            , Canvas.press { x = toFloat x, y = toFloat y }
            )

        Lift ->
            ( Sleeping, Canvas.lift )

        Hover { x, y } ->
            ( state
            , Canvas.hover { x = toFloat x, y = toFloat y }
            )


subs : State -> Sub Msg
subs state =
    Sub.batch
        [ Mouse.moves Hover
        , Mouse.ups <| always Lift
        , Mouse.downs Press
        ]


newInput : Canvas.Input State Msg
newInput =
    { state = Sleeping
    , update = update
    , subs = subs
    }
