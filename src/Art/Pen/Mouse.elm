module Art.Pen.Mouse exposing (Msg, State, update, subs, newState)

import Task
import Mouse
import Art.Box as Box
import Art.Pen as Pen
import Art.Canvas as Canvas
import Art.Canvas exposing (Canvas)
import Art.Stroke exposing (Point)


{-| Translate the given position to be centered on the Canvas before
passign the canvas and the position to the func argument
-}
withMouse : (Point -> Canvas -> a) -> Mouse.Position -> Box.Position -> Canvas -> a
withMouse func mousepos boxpos canvas =
    let
        x =
            toFloat mousepos.x

        y =
            toFloat mousepos.y

        w =
            .w Canvas.size |> toFloat

        h =
            .h Canvas.size |> toFloat

        canvasCoord : Point
        canvasCoord =
            { x = x - (boxpos.x + (w / 2))
            , y = negate (y - (boxpos.y + (h / 2)))
            }
    in
        func canvasCoord canvas


type MouseState
    = Dragging
    | Free


type alias State =
    { mouse : MouseState
    , canvaspos : Box.Position
    , mousepos : Mouse.Position
    }


type Msg
    = DrawAt State
    | LiftMouse State
    | CheckPosition State Mouse.Position
    | CanvasLoc State Box.Position


newState : State
newState =
    State Free (Box.Position 100 0) (Mouse.Position 0 0)


update : Msg -> Canvas -> ( ( State, Canvas ), Cmd Msg )
update msg canvas =
    case msg of
        DrawAt { canvaspos, mousepos } ->
            ( State Dragging canvaspos mousepos
            , withMouse Canvas.draw mousepos canvaspos canvas
            )
                |> Pen.mute

        LiftMouse state ->
            ( { state | mouse = Free }
            , Canvas.lift canvas
            )
                |> Pen.mute

        CheckPosition state newmousepos ->
            ( ( { state | mousepos = newmousepos }, canvas )
            , Box.checkPosition Canvas.id
            )

        CanvasLoc state newposition ->
            let
                newstate =
                    { state | canvaspos = newposition }
            in
                ( ( newstate, canvas )
                , Task.perform identity (Task.succeed <| DrawAt newstate)
                )


subs : State -> Sub Msg
subs state =
    let
        mouseToState : Mouse.Position -> State
        mouseToState pos =
            { state | mousepos = pos }

        boxToState : Box.Position -> State
        boxToState pos =
            { state | canvaspos = pos }

        movementSub =
            case state.mouse of
                -- NOTE: if canvas move while drawing, introduces offset
                Dragging ->
                    Mouse.moves (DrawAt << mouseToState)

                Free ->
                    Sub.none
    in
        Sub.batch
            [ movementSub
            , Mouse.ups (LiftMouse << mouseToState)
            , Mouse.downs (CheckPosition state)
            , Box.subPosition (DrawAt << boxToState)
            ]
