port module Art.Box exposing (Point, Box, checkCanvasBox, sub)

{-| Helper functions to deal with various input types.
-}


{-| sends outgoing message that will eventually trigger the listenCanvasBox
port subsription.
-}
port checkCanvasBox : () -> Cmd msg


{-| Fetch the position of the element with the id that was last
given with the "checkCanvasBox" Command. The input type is a four
element list formated as follow: [x, y, width, height].
-}
port listenCanvasBox : (List Float -> msg) -> Sub msg


{-| The bounding box of an html element.
Centered in {x,y} and with a certain width and height.
-}
type alias Point =
    { x : Float, y : Float }


type alias Box =
    { x : Float, y : Float, width : Float, height : Float }


{-| Wrapper around the checkPosition port to provide a simpler
api
-}
readBox : List Float -> Box
readBox jslist =
    case jslist of
        [ x, y, width, height ] ->
            { x = x, y = y, width = width, height = height }

        _ ->
            { x = 0, y = 0, width = 0, height = 0 }


{-| Calls callback with the given Position offset so it is centered on Box
-}
sub : (Box -> msg) -> Sub msg
sub callback =
    listenCanvasBox (readBox >> callback)
