module Art.RenderedCanvas exposing (htmlCanvas)

import Html exposing (..)
import Collage exposing (traced, path, toForm, filled)
import Element as GraphElement
import List.Nonempty as NE exposing (Nonempty)
import Art.Canvas as Canvas
import Art.Canvas exposing (Canvas)
import Art.Stroke as Stoke
import Art.Stroke exposing (Stroke)


collage :
    { a | width : Float, height : Float }
    -> List Collage.Form
    -> GraphElement.Element
collage { width, height } =
    let
        width_ =
            round width

        height_ =
            round height
    in
        Collage.collage width_ height_


pointToTuple : { a | x : Float, y : Float } -> ( Float, Float )
pointToTuple sp =
    ( sp.x, sp.y )


strokeToForm : Stroke -> Collage.Form
strokeToForm { points, color, size } =
    if NE.isSingleton points then
        Collage.circle (size / 2)
            |> filled color
            |> (Collage.move <| pointToTuple <| NE.head points)
    else
        points
            |> NE.map pointToTuple
            |> NE.toList
            |> path
            |> traced
                { color = color
                , width = size
                , cap = Collage.Round
                , join = Collage.Smooth
                , dashing = []
                , dashOffset = 0
                }


canvasToForm : Canvas -> List Collage.Form
canvasToForm canvas =
    Canvas.strokes canvas
        |> List.reverse
        |> List.map strokeToForm


htmlCanvas : Canvas -> Html msg
htmlCanvas canvas =
    canvasToForm canvas
        |> collage canvas.box
        |> GraphElement.toHtml
