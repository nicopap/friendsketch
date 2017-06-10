module Art.Pen exposing (checkPosition, mute)

{-| Specifications that something must follow in order to be a Pen for drawing
that can draw on a Canvas
-}

import Art.Canvas exposing (Canvas)
import Art.Canvas as Canvas
import Art.Box as Box


{-| Convinience function to reduce the verbosity returning a Cmd.none
-}
mute : zz -> ( zz, Cmd msg )
mute leftside =
    ( leftside, Cmd.none )


checkPosition : Cmd msg
checkPosition =
    Box.checkPosition Canvas.id
