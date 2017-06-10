module Art.Stroke exposing (Stroke, Point, new, draw)

import Color
import Color exposing (Color)
import List.Nonempty as NE exposing (Nonempty, (:::))


draw : Stroke -> Point -> Stroke
draw =
    drawatthreashold 10.0


type alias Point =
    { x : Float
    , y : Float
    }


type alias Stroke =
    { points : Nonempty Point
    , color : Color
    , size : Float
    }


new : { a | x : Float, y : Float } -> Color -> Float -> Stroke
new { x, y } color size =
    Stroke (NE.fromElement (Point x y)) color size


dist : Point -> Point -> Float
dist p1 p2 =
    (p2.x - p1.x) ^ 2 + (p2.y - p1.y) ^ 2


drawatthreashold : Float -> Stroke -> Point -> Stroke
drawatthreashold threshold stroke newPoint =
    let
        lastPoint =
            NE.head stroke.points
    in
        if (dist lastPoint newPoint) > threshold then
            { stroke | points = NE.cons newPoint stroke.points }
        else
            stroke
