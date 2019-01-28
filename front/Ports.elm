port module Ports exposing (openLink, stashAndOpen, copyCatch, bottomScrollChat)

import Json.Encode exposing (Value)


port openLink : String -> Cmd msg


port stashAndOpen : ( List ( String, Value ), String ) -> Cmd msg


port copyCatch : ( String, String ) -> Cmd msg


port bottomScrollChat : () -> Cmd msg
