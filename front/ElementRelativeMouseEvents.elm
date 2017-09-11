{- https://github.com/Elm-Canvas/element-relative-mouse-events
   original Author?: Chadtech.
-}


module ElementRelativeMouseEvents
    exposing
        ( onMouseDown
        , onMouseUp
        , onMouseMove
        , onClick
        , onDoubleClick
        , onMouseEnter
        , Point
        )

{-| These functions are just like the `Html.Events` functions `onMouseDown`, `onMouseUp`, etc, except that they pass along a `Point`, representing exactly where on the html element the mouse activity occured.

@docs Point, onMouseDown, onMouseUp, onMouseMove, onClick, onDoubleClick

-}

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json
import Bitwise exposing (and)


{-| This is the basic type we use to represent where a mouse event happens on the screen
-}
type alias Point =
    { x : Float
    , y : Float
    }


type alias EventCoordinates =
    ( ( Float, Float ), ( Float, Float ), ( Float, Float ), ( Float, Float ) )


{-| Just like the `onMouseDown` in `Html.Events`, but this one passes along a `Point` that is relative to the html element. So clicking right in the middle of a 200x200 div will return `Point 100 100`.

    Canvas.toHtml
        [ Canvas.Events.onClick CanvasClick ]
        canvas

    -- ..

    case message of
        CanvasClick point ->
            -- ..

-}
onMouseDown : (Point -> msg) -> Attribute msg
onMouseDown message =
    on "mousedown" <|
        Json.map
            (positionInCanvas >> message)
            positionDecoder


{-| -}
onMouseUp : (Point -> msg) -> Attribute msg
onMouseUp message =
    on "mouseup" <|
        Json.map
            (positionInCanvas >> message)
            positionDecoder


{-| Will call message with a tuple the first element of which is the location
relative to the html element of the cursor; The second element is a boolean
which is True if the left mouse button is pressed otherwise False
-}
onMouseEnter : (( Point, Bool ) -> msg) -> Attribute msg
onMouseEnter message =
    on "mouseenter" <|
        Json.map
            (Tuple.mapFirst positionInCanvas >> message)
            positionNpressDecoder


{-| -}
onMouseMove : (Point -> msg) -> Attribute msg
onMouseMove message =
    on "mousemove" <|
        Json.map
            (positionInCanvas >> message)
            positionDecoder


{-| -}
onClick : (Point -> msg) -> Attribute msg
onClick message =
    on "click" <|
        Json.map
            (positionInCanvas >> message)
            positionDecoder


{-| -}
onDoubleClick : (Point -> msg) -> Attribute msg
onDoubleClick message =
    on "dblclick" <|
        Json.map
            (positionInCanvas >> message)
            positionDecoder


positionInCanvas : EventCoordinates -> Point
positionInCanvas ( client, offset, body, documentElement ) =
    let
        ( cx, cy ) =
            client

        ( ox, oy ) =
            offset

        ( bx, by ) =
            body

        ( dx, dy ) =
            documentElement
    in
        Point ((cx + bx + dx) - ox) ((cy + by + dy) - oy)


positionDecoder : Json.Decoder EventCoordinates
positionDecoder =
    Json.map4 (,,,)
        (toTuple [ "clientX" ] [ "clientY" ])
        (toTuple [ "currentTarget", "offsetLeft" ] [ "currentTarget", "offsetTop" ])
        (toTuple [ "view", "document", "body", "scrollLeft" ] [ "view", "document", "body", "scrollTop" ])
        (toTuple [ "view", "document", "documentElement", "scrollLeft" ] [ "view", "document", "documentElement", "scrollTop" ])


{-| Test if the first bit of the `buttons` value is set, if
so, it means the leftmousebutton is pressed.
-}
buttonDecoder : Json.Decoder Bool
buttonDecoder =
    Json.map
        (and 1 >> (==) 1)
        (Json.field "buttons" Json.int)


positionNpressDecoder : Json.Decoder ( EventCoordinates, Bool )
positionNpressDecoder =
    Json.map2 (,) positionDecoder buttonDecoder


toTuple : List String -> List String -> Json.Decoder ( Float, Float )
toTuple x y =
    Json.map2 (,) (Json.at x Json.float) (Json.at y Json.float)
