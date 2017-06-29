port module Art.Box exposing (Point, Box, checkCanvas, sub)

{-| sends outgoing message that will eventually trigger the listenCanvasBox
port subsription.
-}


port checkCanvas : () -> Cmd msg


{-| Fetch the position of the element with the id that was last
given with the "checkCanvas" Command. The input type is a four
element list formated as follow: [x, y, width, height].
-}
port listenCanvas : (List Float -> msg) -> Sub msg


type alias Located a =
    { a | x : Float, y : Float }


type alias Sized a =
    { a | width : Float, height : Float }


type alias Point =
    Located {}


{-| The bounding box of an html element.
Centered in {x,y} and with a certain width and height.
-}
type alias Box =
    Located (Sized {})


{-| Wrapper around the checkPosition port to provide a simpler
api
-}
read : List Float -> Box
read jslist =
    case jslist of
        [ x, y, width, height ] ->
            { x = x, y = y, width = width, height = height }

        _ ->
            { x = 0, y = 0, width = 0, height = 0 }


{-| Calls callback with the given Position offset so it is centered on Box
-}
sub : (Box -> msg) -> Sub msg
sub callback =
    listenCanvas (read >> callback)
