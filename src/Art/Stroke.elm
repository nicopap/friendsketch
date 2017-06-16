module Art.Stroke exposing (Stroke, new, draw)

{-| Represents a simple paint stroke.


# Definition

@docs Stroke, new


# Modyfing a stroke

@docs draw

-}

import Color
import Color exposing (Color)
import List.Nonempty as NE exposing (Nonempty)
import Art.Box exposing (Point)


{-| Expand a stroke to include a new point.
-}
draw : Point -> Stroke -> Stroke
draw point stroke =
    drawatthreashold (stroke.size / 2) point stroke


{-| A Stroke, can be of any Color and has a given size.
-}
type alias Stroke =
    { points : Nonempty Point
    , color : Color
    , size : Float
    }


{-| A stroke starting at a given Point, with given color and size.
-}
new : Point -> Color -> Float -> Stroke
new point color size =
    Stroke (NE.fromElement point) color size


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
