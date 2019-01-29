module Chat.Message exposing (Message, view, into)

import API exposing (ChatContent, Name)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Message
    = Message_ { author : String , content : String }
    | Left String
    | Joined String


into : API.GameEvent -> Message
into event =
    case event of
        API.EventLeft name ->
            Left <| API.showName name

        API.EventJoined name ->
            Joined <| API.showName name

        API.EventMessage { content, author } ->
            Message_
                { content = API.showChatContent content
                , author = API.showName author
                }


view : Message -> Html msg
view message =
    case message of
        Message_ { author, content } ->
            div [ class "text message" ] [ text <| author ++ ": " ++ content ]

        Left name ->
            div [ class "leave message" ] [ text <| name ++ " left" ]

        Joined name ->
            div [ class "join message" ] [ text <| name ++ " joined" ]

