module Chat.Message exposing (Message, decode, view)

import Json.Decode exposing (map3, field, string, decodeString, Decoder)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type alias Message =
    { author : String
    , date : String
    , content : String
    }


decodeMessage : Decoder Message
decodeMessage =
    map3 Message
        (field "Author" string)
        (field "Date" string)
        (field "Content" string)


decode : String -> Message
decode json =
    case decodeString decodeMessage json of
        Ok message ->
            message

        Err errormsg ->
            errormessage errormsg


errormessage : String -> Message
errormessage errormsg =
    Message "SERVER" "" ("ERROR: " ++ errormsg)


view : Message -> Html msg
view message =
    div [ class "message" ]
        [ b [] [ text message.author ]
        , text (" : " ++ message.content)
        ]
