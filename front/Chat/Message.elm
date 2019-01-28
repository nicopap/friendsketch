module Chat.Message exposing (Message, insert, view, into)

import API exposing (ChatContent, Name)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Message =
    Message_
        { author : String
        , content : String
        , order : Int
        }


insert : API.ChatMsg_ -> List Message -> List Message
insert message history =
    case history of
        [] ->
            [ into message ]

        Message_ head :: tail ->
            if head.order > message.order then
                Message_ head :: insert message tail
            else
                into message :: Message_ head :: tail


into : API.ChatMsg_ -> Message
into { content, author, order } =
    Message_
        { content = API.showChatContent content
        , author = API.showName author
        , order = order
        }


view : Message -> Html msg
view (Message_ message) =
    div [ class "message" ]
        [ text <| message.author ++ " : " ++ message.content
        ]
