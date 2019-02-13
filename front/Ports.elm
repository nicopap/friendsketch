port module Ports exposing (openLink, stashAndOpen, selectRoomid, bottomScrollChat)

import Json.Encode exposing (Value)


port openLink : String -> Cmd msg


port stashAndOpen : ( String, List ( String, Value ) ) -> Cmd msg


port selectRoomid : () -> Cmd msg


port bottomScrollChat : () -> Cmd msg
