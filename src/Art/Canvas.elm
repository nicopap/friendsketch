module Art.Canvas
    exposing
        ( Canvas
        , new
        , id
        , draw
        , drawAbsolute
        , lift
        , selectColor
        , selectSize
        , setLocation
        , strokes
        )

{-| The data structure and functions related to drawing on a surface. This
module only exposes very basic operations in order to let other implement
the actual way of drawing or how to display the drawing.


# Definition

@docs Canvas, new


# Constants

@docs id


# Operations on Canvas

@docs draw, drawAbsolute, lift, selectColor, selectSize


# Informations on the Canvas

@docs strokes

-}

import Color
import Color exposing (Color)
import Art.Stroke as Stroke
import Art.Stroke exposing (Stroke)
import Art.Box exposing (Box, Point)
import Art.Box as Box


{-| The html id of the canvas. ideally this gets abstracted away for a more
flexible solution
-}
id : String
id =
    "drawingspace"


{-| The internal state of the Canvas: Or the user is drawing physically,
which means it is using the Canvas. Or it uses some other tools, maybe to
select a pen.
-}
type State
    = Drawing Stroke
    | Selecting


{-| Describes a Canvas. It only agregates operations on it, do not have any
"concrete" representation.
-}
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


{-| Get A list of all the strokes on the canvas.
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


{-| Get various states about a canvas.
-}
height : Canvas -> Float
height =
    .box >> .height >> (*) 0.5


width : Canvas -> Float
width =
    .box >> .width >> (*) 0.5


xcenter : Canvas -> Float
xcenter =
    .box >> .x


ycenter : Canvas -> Float
ycenter =
    .box >> .y


{-| A default canvas.
-}
new : Canvas
new =
    Canvas [] Selecting Color.black 20 { x = 355, y = 303, width = 600, height = 400 }
