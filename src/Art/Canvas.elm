module Art.Canvas
    exposing
        ( Canvas
        , new
        , view
        , update
        , Msg(..)
        , Input
        , updateInput
        , draw
        , drawAbsolute
        , lift
        , selectColor
        , selectSize
        , setLocation
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
    = Drawing Stroke
    | Selecting


type alias Canvas =
    { strokes : List Stroke
    , state : State
    , color : Color
    , strokeSize : Float
    , box : Box
    }


{-| Get A list of all the strokes on the canvas
-}
strokes : Canvas -> List Stroke
strokes canvas =
    case canvas.state of
        Drawing lastStroke ->
            lastStroke :: canvas.strokes

        Selecting ->
            canvas.strokes


{-| Change the selected color of the canvas
-}
selectColor : Color -> Canvas -> Canvas
selectColor newcolor canvas =
    { canvas | color = newcolor }


{-| Change the pen size of the canvas
-}
selectSize : Float -> Canvas -> Canvas
selectSize newsize canvas =
    { canvas | strokeSize = newsize }


{-| Change the location of the canavs
-}
setLocation : Box -> Canvas -> Canvas
setLocation newloc canvas =
    { canvas | box = newloc }


{-| A default canvas
-}
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
    in
        canvasToForm canvas
            |> Collage.collage (rwidth canvas) (rheight canvas)
            |> GraphElement.toHtml
            |> List.singleton
            >> div [ id "drawingarea" ]



-- UPDATE


{-| Draws at position on Canvas, position is centered on canvas

    draw position canvas

Will draw at a given point on the Canvas. If there is a stroke being drawn,
it will continue it, otherwise it creates a new one with the colors and size
that is specified in the Canvas.

-}
draw_ : Point -> Canvas -> Canvas
draw_ position canvas =
    let
        height = .box >> .height >> (*) 0.5
        width = .box >> .width >> (*) 0.5
        outbound { x, y } cvs = abs x > width cvs || abs y > height cvs
    in
        if outbound position canvas then
            lift_ canvas
        else
            case canvas.state of
                Selecting ->
                    let
                        newstate { color, strokeSize } =
                            Drawing (Stroke.new position color strokeSize)
                    in
                        { canvas | state = newstate canvas }

                Drawing stroke ->
                    { canvas | state = Drawing (Stroke.draw position stroke) }


{-| Draw translating point from page coordinates to canvas coordinates.

    drawAbsolute point canvas

Same as `draw` but accounts for offset introduced by the page.

-}
drawAbsolute_ : Point -> Canvas -> Canvas
drawAbsolute_ pos canvas =
    let
        xcenter = .box >> .x
        ycenter = .box >> .y
        offsetPosition =
            { x = pos.x - xcenter canvas
            , y = ycenter canvas - pos.y
            }
    in
        draw_ offsetPosition canvas


{-| Lift the "pen" from the canvas, that means the stroke is finished.

    lift canvas

Ends the current stroke, the next time you `draw` on the canvas, it will be
concidered a new stroke.

-}
lift_ : Canvas -> Canvas
lift_ canvas =
    case canvas.state of
        Selecting ->
            canvas

        Drawing stroke ->
            { canvas
                | strokes = stroke :: canvas.strokes
                , state = Selecting
            }


update : Msg -> Canvas -> Canvas
update msg =
    case msg of
        Draw point ->
            draw_ point

        DrawAbsolute point ->
            drawAbsolute_ point

        Lift ->
            lift_



-- EMISSION functions to communicate with a Canvas.


type Msg
    = Draw Point
    | DrawAbsolute Point
    | Lift


emit = Task.succeed >> Task.perform identity

lift : Cmd Msg
lift = emit Lift

draw : Point -> Cmd Msg
draw = emit << Draw

drawAbsolute : Point -> Cmd Msg
drawAbsolute = emit << DrawAbsolute


{-| The pen implementation.
-}
type alias Input s m =
    { state : s
    , update : m -> s -> ( s, Cmd Msg )
    , subs : s -> Sub m
    }


{-| Run the input's update function and transforms its state
-}
updateInput : m -> Input s m -> ( Input s m, Cmd Msg )
updateInput msg input =
    input.update msg input.state
        |> \( state, cmd ) -> ( { input | state = state }, cmd )
