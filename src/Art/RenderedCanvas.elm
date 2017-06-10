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


{-| TODO: decouple canvas size from this module
-}
collage : List Collage.Form -> GraphElement.Element
collage =
    Collage.collage (.w Canvas.size) (.h Canvas.size)


pointToTuple : { a | x : Float, y : Float } -> ( Float, Float )
pointToTuple sp =
    ( sp.x, sp.y )


strokeToForm : Stroke -> Collage.Form
strokeToForm { points, color, size } =
    if NE.isSingleton points then
        Collage.circle 1
            |> filled color
            |> Collage.move (NE.head points |> pointToTuple)
    else
        points
            |> NE.map pointToTuple
            |> NE.toList
            |> path
            |> traced Collage.defaultLine


canvasToForm : Canvas -> List Collage.Form
canvasToForm canv =
    let
        mapon =
            List.map strokeToForm
    in
        case canv.currentStroke of
            Just stroke ->
                mapon (stroke :: canv.strokes)

            Nothing ->
                mapon canv.strokes


htmlCanvas : Canvas -> Html msg
htmlCanvas canvas =
    canvasToForm canvas
        |> collage
        |> GraphElement.toHtml
