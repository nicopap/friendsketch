module Art.Canvas
    exposing
        ( Canvas
        , new
        , view
        , update
        , Msg
        , Input
        , Tool
        , mapInput
        , viewTool
        , subInput
        , lift
        , press
        , hover
        , changeColor
        , changePenSize
        )

import Task
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Collage
import Element as GraphElement
import List.Nonempty as NE exposing (Nonempty)
import Color exposing (Color)
import Art.Stroke as Stroke exposing (Stroke)
import Art.Box as Box exposing (Box, Point)


type State
    = Drawing (Maybe Stroke)
    | Selecting
    | Hovering Point


type alias Canvas =
    { strokes : List Stroke
    , state : State
    , color : Color
    , strokeSize : Float
    , box : {width : Float, height : Float}
    }


{-| A default canvas -}
new : Canvas
new =
    Canvas [] Selecting Color.black 20 { width = 600, height = 400 }


-- VIEW


strokeToForm : Stroke -> Collage.Form
strokeToForm { points, color, size } =
    if NE.isSingleton points then
        Collage.circle (size / 2)
            |> Collage.filled color
            |> (Collage.move <| (\{ x, y } -> ( x, y )) <| NE.head points)
    else
        points
            |> NE.map (\{ x, y } -> ( x, y ))
            |> NE.toList
            |> Collage.path
            |> Collage.traced
                { color = color
                , width = size
                , cap = Collage.Round
                , join = Collage.Smooth
                , dashing = []
                , dashOffset = 0
                }


canvasToForm : Canvas -> List Collage.Form
canvasToForm { state, strokeSize, strokes, color } =
    let
        cursorView =
            case state of
                Hovering { x, y } ->
                    [ Collage.circle (strokeSize / 2)
                        |> Collage.outlined
                            { color = color
                            , width = 1
                            , cap = Collage.Flat
                            , join = Collage.Smooth
                            , dashing = [ 1, round <| strokeSize / 10 ]
                            , dashOffset = 0
                            }
                        |> Collage.move ( x, y )
                    ]

                Drawing (Just stroke) ->
                    [ strokeToForm stroke ]

                Drawing Nothing ->
                    []

                Selecting ->
                    []
    in
        strokes
            |> List.map strokeToForm
            |> (flip (++)) cursorView


view : Canvas -> Html msg
view canvas =
    let
        rheight = .box >> .height >> round
        rwidth = .box >> .width >> round
    in
        canvasToForm canvas
            |> Collage.collage (rwidth canvas) (rheight canvas)
            |> GraphElement.toHtml
            |> List.singleton
            >> div [ id "drawingarea" ]


-- UPDATE


{-| Lift the "pen" from the canvas, that means the stroke is finished -}
lift_ : Canvas -> Canvas
lift_ canvas =
    case canvas.state of
        Drawing (Just stroke) ->
            { canvas
                | strokes = canvas.strokes ++ [ stroke ]
                , state = Selecting
            }

        _ ->
            canvas


hover_ : Point -> Canvas -> Canvas
hover_ point canvas =
    { canvas | state = case canvas.state of
        Drawing Nothing ->
            let
                size = canvas.strokeSize
                color = canvas.color
            in
                Drawing <| Just <| Stroke.new point color size

        Drawing (Just stroke) ->
            Drawing <| Just <| Stroke.draw point stroke

        _ ->
            Hovering point
    }


{-| Wraps a function that takes a Point and modifies a Canvas such as
the result has the function applied only if the point is within bounds
of the canvas.
-}
boxed : (Point -> Canvas -> Canvas) -> Point -> Canvas -> Canvas
boxed f point canvas =
    let
        height = .box >> .height >> (*) 0.5
        width = .box >> .width >> (*) 0.5
        outbound { x, y } cvs = abs x > width cvs || abs y > height cvs
    in
        if outbound point canvas then
            lift_ canvas
        else
            f point canvas


type Msg
    = Hover Point
    | Lift
    | Press Point
    | ChangeColor Color
    | ChangePenSize Float


update : Msg -> Canvas -> Canvas
update msg canvas =
    case msg of
        Hover point ->
            ( boxed <| hover_) point canvas

        Press point ->
            ( boxed <| hover_) point { canvas | state = Drawing Nothing }

        Lift ->
            lift_ canvas

        ChangeColor newcolor ->
            { canvas | color = newcolor }

        ChangePenSize newsize ->
            { canvas | strokeSize = newsize }


-- EMISSION functions to communicate with a Canvas.
{-| A generic Canvas modification tool -}
type alias CanvasTool a s m =
    { a
        | state : s
        , update : m -> s -> ( s, Cmd Msg )
    }


emit = Task.succeed >> Task.perform identity

lift = emit Lift
press = emit << Press
hover = emit << Hover
changePenSize = emit << ChangePenSize
changeColor = emit << ChangeColor


{-| An input that can modify the Canvas -}
type alias Input s m =
    CanvasTool {subs : s -> Sub m} s m

{-| A Canvas modification tool that is visible -}
type alias Tool s m =
    CanvasTool { view : s -> Html m } s m


mapInput : (CanvasTool a s m -> x) -> (Msg -> y) -> m -> CanvasTool a s m -> (x, Cmd y)
mapInput maptool mapcmd msg tool =
    tool.update msg tool.state
        |> \(state, cmd) -> ( maptool { tool | state = state }, Cmd.map mapcmd cmd )

viewTool : (m -> x) -> Tool s m -> Html x
viewTool mapf tool =
    Html.map mapf (tool.view tool.state)

subInput : (m -> x) -> Input s m -> Sub x
subInput mapf input =
    Sub.map mapf (input.subs input.state)

