port module Ports exposing (stashAndOpen)

import Json.Encode exposing (Value)

port stashAndOpen : ( String, List ( String, Value ) ) -> Cmd msg
