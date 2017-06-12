module Art.Canvas
    exposing
        ( Canvas
        , draw
        , drawAbsolute
        , lift
        , strokes
        , selectColor
        , selectSize
        , new
        , id
        )

import Task
import Color
import Color exposing (Color)
import Json.Decode exposing (Decoder, map2, float, decodeString, field)
import Art.Stroke as Stroke
import Art.Stroke exposing (Stroke)
import Art.Box exposing (Box, Point)
import Art.Box as Box


-- Constants


id =
    "drawingspace"


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
-}
draw : Point -> Canvas -> Canvas
draw position canvas =
    if abs position.x > width canvas || abs position.y > height canvas then
        canvas
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


{-| Lift the "pen" from the canvas, that means the stroke is finished
-}
lift : Canvas -> Canvas
lift canvas =
    case canvas.state of
        Selecting ->
            Debug.log "Lifting on a Selecting canvas shouldn't happen" canvas

        Drawing stroke ->
            { canvas
                | strokes = stroke :: canvas.strokes
                , state = Selecting
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


{-| Get various states about a canvas.
-}
height =
    .box >> .height


width =
    .box >> .width


xcenter =
    .box >> .x


ycenter =
    .box >> .y


{-| A default canvas
-}
new : Canvas
new =
    Canvas [] Selecting Color.black 20 { x = 0, y = 0, width = 600, height = 400 }
