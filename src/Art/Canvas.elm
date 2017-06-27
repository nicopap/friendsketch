module Art.Canvas
    exposing
        ( Canvas
        , new
        , view
        , update
        , Msg(..)
        , Input
        , updateInput
        , lift
        , press
        , hover
        , selectColor
        , selectSize
        , setLocation
        )

import Task
import Html exposing (Html, div)
import Html.Attributes exposing (id, style)
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
    , box : Box
    }


{-| Get A list of all the strokes on the canvas -}
strokes : Canvas -> List Stroke
strokes canvas =
    case canvas.state of
        Drawing (Just lastStroke) ->
            lastStroke :: canvas.strokes

        _ ->
            canvas.strokes


{-| Change the selected color of the canvas -}
selectColor : Color -> Canvas -> Canvas
selectColor newcolor canvas =
    { canvas | color = newcolor }


{-| Change the pen size of the canvas -}
selectSize : Float -> Canvas -> Canvas
selectSize newsize canvas =
    { canvas | strokeSize = newsize }


{-| Change the location of the canvas -}
setLocation : Box -> Canvas -> Canvas
setLocation newloc canvas =
    { canvas | box = newloc }


{-| A default canvas -}
new : Canvas
new =
    Canvas [] Selecting Color.black 20 { x = 355, y = 303, width = 600, height = 400 }



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
canvasToForm canvas =
    strokes canvas
        |> List.reverse
        |> List.map strokeToForm


view : Canvas -> Html msg
view canvas =
    let
        rheight = .box >> .height >> round
        rwidth = .box >> .width >> round
        borderStyle state =
            style
                [ ("borderColor", case state of
                    Drawing (Just _) ->
                        "green"
                    Drawing Nothing ->
                        "blue"
                    Selecting ->
                       "red"
                    Hovering _ ->
                        "black"
                )
                , ( "borderStyle", "solid")
                , ( "width", "600px")
                , ("margin", "auto")
                , ("marginTop", "100px")
                ]
    in
        canvasToForm canvas
            |> Collage.collage (rwidth canvas) (rheight canvas)
            |> GraphElement.toHtml
            |> List.singleton
            >> div [borderStyle canvas.state, id "drawingarea" ]



-- UPDATE


{-| Lift the "pen" from the canvas, that means the stroke is finished -}
lift_ : Canvas -> Canvas
lift_ canvas =
    case canvas.state of
        Drawing (Just stroke) ->
            { canvas | strokes = stroke :: canvas.strokes
                     , state = Selecting }

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


{-| Wraps a function that takes a point and modifies a Canvas so the point
is applied with the offset induced by that poisition of the canvas on the
page.
-}
offset : (Point -> Canvas -> Canvas) -> Point -> Canvas -> Canvas
offset f point canvas =
    let
        xcenter = .box >> .x
        ycenter = .box >> .y
        offsetPosition =
            { x = point.x - xcenter canvas
            , y = ycenter canvas - point.y
            }
    in
        f offsetPosition canvas


type Msg
    = Hover Point
    | Lift
    | Press Point


update : Msg -> Canvas -> Canvas
update msg canvas =
    case msg of
        Hover point ->
            (offset <| boxed <| hover_) point canvas

        Press point ->
            (offset <| boxed <| hover_) point {canvas | state = Drawing Nothing}

        Lift ->
            lift_ canvas


-- EMISSION functions to communicate with a Canvas.


emit = Task.succeed >> Task.perform identity

lift : Cmd Msg
lift = emit Lift

press : Point -> Cmd Msg
press = emit << Press

hover : Point -> Cmd Msg
hover = emit << Hover


{-| The pen implementation -}
type alias Input s m =
    { state : s
    , update : m -> s -> ( s, Cmd Msg )
    , subs : s -> Sub m
    }


{-| Run the input's update function and transforms its state -}
updateInput : m -> Input s m -> ( Input s m, Cmd Msg )
updateInput msg input =
    input.update msg input.state
        |> \( state, cmd ) -> ( { input | state = state }, cmd )
