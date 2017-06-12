module Art.Stroke exposing (Stroke, new, draw)

import Color
import Color exposing (Color)
import List.Nonempty as NE exposing (Nonempty, (:::))
import Art.Box exposing (Point)


-- draw : Point -> Stroke -> Stroke
-- draw =
--     drawatthreashold 10.0


draw : Point -> Stroke -> Stroke
draw newpoint oldstroke =
    { oldstroke | points = NE.cons newpoint oldstroke.points }


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


drawatthreashold : Float -> Point -> Stroke -> Stroke
drawatthreashold threshold newpoint stroke =
    let
        lastpoint =
            NE.head stroke.points
    in
        if (dist lastpoint newpoint) > threshold then
            { stroke | points = NE.cons newpoint stroke.points }
        else
            stroke
