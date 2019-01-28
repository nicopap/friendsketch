module Chat.Message exposing (Message, insert, view, into)

import API exposing (ChatContent, Name)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Message =
    Message_
        { author : String
        , content : String
        }


insert : API.ChatMsg_ -> List Message -> List Message
insert message history =
    into message :: history


into : API.ChatMsg_ -> Message
into { content, author } =
    Message_
        { content = API.showChatContent content
        , author = API.showName author
        }


view : Message -> Html msg
view (Message_ message) =
    div [ class "message" ]
        [ text <| message.author ++ " : " ++ message.content
        ]
