port module Ports exposing (openLink, stashAndOpen)


port openLink : String -> Cmd msg


port stashAndOpen : ( List ( String, String ), String ) -> Cmd msg
