module Art.Canvas exposing (draw, lift, Canvas, new, id, size)

import Task
import Color
import Json.Decode exposing (Decoder, map2, float, decodeString, field)
import Art.Stroke as Stroke
import Art.Stroke exposing (Stroke, Point)


-- Constants


id =
    "drawingspace"


size : { w : Int, h : Int }
size =
    { w = 400
    , h = 400
    }


{-| Describes a Canvas. It only agregates operations on it, do not have any
"concrete" representation.
-}
type alias Canvas =
    { strokes : List Stroke
    , currentStroke : Maybe Stroke
    }


{-| Draws at position on Canvas, position is centered on canvas
-}
draw : Point -> Canvas -> Canvas
draw position canvas =
    case canvas.currentStroke of
        Nothing ->
            { canvas | currentStroke = Stroke.new position Color.black 10 |> Just }

        Just stroke ->
            { canvas | currentStroke = Stroke.draw stroke position |> Just }


{-| Lift the "pen" from the canvas, that means the stroke is finished
-}
lift : Canvas -> Canvas
lift canvas =
    case canvas.currentStroke of
        Nothing ->
            canvas

        Just stroke ->
            { canvas
                | strokes = stroke :: canvas.strokes
                , currentStroke = Nothing
            }


{-| A default canvas
-}
new : Canvas
new =
    Canvas [] Nothing
