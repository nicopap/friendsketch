module Art.RenderedCanvas exposing (htmlCanvas)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
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
            |> Collage.move (NE.head points |> pointToTuple)
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
    List.map strokeToForm (Canvas.strokes canvas)


htmlCanvas : Canvas -> Html msg
htmlCanvas canvas =
    canvasToForm canvas
        |> collage canvas.box
        |> GraphElement.toHtml
