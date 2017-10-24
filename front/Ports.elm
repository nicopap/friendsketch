port module Ports exposing (openLink, stashAndOpen, copyCatch)


port openLink : String -> Cmd msg


port stashAndOpen : ( List ( String, String ), String ) -> Cmd msg


port copyCatch : ( String, String ) -> Cmd msg
