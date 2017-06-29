module Chat.InputField exposing (prepareMessage)

{-| This module formalizes the communication of the chat messages to the
server.
-}

import Time exposing (Time)
import Time
import Task
import Json.Encode exposing (encode, float, string, object)


{-| Translates the request into a machine-readable request.
-}
prepareMessage : String -> (String -> msg) -> Cmd msg
prepareMessage message callback =
    let
        jsonWithmsg : Time -> String
        jsonWithmsg time =
            encode 0
                (object
                    [ ( "timestamp", float <| Time.inMilliseconds time )
                    , ( "message", string message )
                    ]
                )
    in
        Cmd.map
            callback
            (Task.perform jsonWithmsg Time.now)
