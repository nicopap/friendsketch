port module Art.Box exposing (Box, Position, checkPosition, readPosition, readBox, subPosition, sub)

{-| Helper functions to deal with various input types.
-}


{-| Fetch the position of the element with the id that was last
given with the "checkPosition" Command. The input type is a four
element list formated as follow: [left, right, top, bottom].
-}
port position : (List Float -> msg) -> Sub msg


{-| Commands javascript to resend the bounding box of the element
with String #id.
-}
port checkPosition : String -> Cmd msg


{-| Sometimes you just need the top left point of a box to judge
where an element is.
-}
type alias Position =
    { x : Float
    , y : Float
    }


{-| The bounding box of an html element.
-}
type alias Box =
    { left : Float
    , right : Float
    , top : Float
    , bottom : Float
    }


{-| Wrapper around the checkPosition port to provide a simpler
api
-}
readBox : List Float -> Box
readBox jslist =
    case jslist of
        [ left, right, top, bottom ] ->
            Box left right top bottom

        _ ->
            Box 0 0 0 0


readPosition : List Float -> Position
readPosition jslist =
    case jslist of
        [ left, _, top, _ ] ->
            { x = left, y = top }

        _ ->
            { x = 0, y = 0 }


sub : (Box -> msg) -> Sub msg
sub callback =
    position (readBox >> callback)


subPosition : (Position -> msg) -> Sub msg
subPosition callback =
    position (readPosition >> callback)
