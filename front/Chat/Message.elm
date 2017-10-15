module Chat.Message exposing (Message, decode, view)

import Json.Decode exposing (string, decodeString, Decoder)
import TypeDecoders exposing (..)
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
    Message
        <*| "author" :* string
        |*| "date" :* string
        |*| "content" :* string


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
