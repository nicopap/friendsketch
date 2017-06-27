module Art.Canvas
    exposing
        ( Canvas
        , new
        , view
        , draw
        , drawAbsolute
        , lift
        , selectColor
        , selectSize
        , setLocation
        )


import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Collage
import Element as GraphElement
import List.Nonempty as NE exposing (Nonempty)
import Color exposing (Color)
import Art.Stroke as Stroke exposing (Stroke)
import Art.Box as Box exposing (Box,Point)


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


{-| Draws at position on Canvas, position is centered on canvas

    draw position canvas

Will draw at a given point on the Canvas. If there is a stroke being drawn,
it will continue it, otherwise it creates a new one with the colors and size
that is specified in the Canvas.

-}
draw : Point -> Canvas -> Canvas
draw position canvas =
    if abs position.x > width canvas || abs position.y > height canvas then
        lift canvas
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
drawAbsolute : Point -> Canvas -> Canvas
drawAbsolute pos canvas =
    let
        offsetPosition =
            { x = pos.x - xcenter canvas
            , y = ycenter canvas - pos.y
            }
    in
        draw offsetPosition canvas


{-| Lift the "pen" from the canvas, that means the stroke is finished.

    lift canvas

Ends the current stroke, the next time you `draw` on the canvas, it will be
concidered a new stroke.

-}
lift : Canvas -> Canvas
lift canvas =
    case canvas.state of
        Selecting ->
            canvas

        Drawing stroke ->
            { canvas
                | strokes = stroke :: canvas.strokes
                , state = Selecting
            }


{-| Get A list of all the strokes on the canvas -}
strokes : Canvas -> List Stroke
strokes canvas =
    case canvas.state of
        Drawing lastStroke ->
            lastStroke :: canvas.strokes

        Selecting ->
            canvas.strokes


{-| Change the selected color of the canvas -}
selectColor : Color -> Canvas -> Canvas
selectColor newcolor canvas =
    { canvas | color = newcolor }


{-| Change the pen size of the canvas -}
selectSize : Float -> Canvas -> Canvas
selectSize newsize canvas =
    { canvas | strokeSize = newsize }


{-| Change the location of the canavs -}
setLocation : Box -> Canvas -> Canvas
setLocation newloc canvas =
    { canvas | box = newloc }


{-| Get various stats about a canvas -}
height = .box >> .height >> (*) 0.5
width = .box >> .width >> (*) 0.5
rheight = .box >> .height >> round
rwidth = .box >> .width >> round
xcenter = .box >> .x
ycenter = .box >> .y


{-| A default canvas -}
new : Canvas
new =
    Canvas [] Selecting Color.black 20 { x = 355, y = 303, width = 600, height = 400 }


strokeToForm : Stroke -> Collage.Form
strokeToForm { points, color, size } =
    if NE.isSingleton points then
        Collage.circle (size / 2)
            |> Collage.filled color
            |> (Collage.move <| (\{x,y} -> (x,y)) <| NE.head points)
    else
        points
            |> NE.map (\{x,y} -> (x,y))
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
    canvasToForm canvas
        |> Collage.collage (rwidth canvas) (rheight canvas)
        |> GraphElement.toHtml
        |> (\x -> div [ id "drawingarea" ] [x])
