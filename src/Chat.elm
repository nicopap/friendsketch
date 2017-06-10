module Chat exposing (Chat, subs, new, Msg, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Chat.Room as Room
import Chat.Room exposing (Room)


type alias Chat =
    { connection : Connection
    }


type Connection
    = UpIn Room
    | Down


subs : Chat -> Sub Msg
subs chat =
    case chat.connection of
        UpIn room ->
            Sub.batch
                [ Sub.map RoomMsg (Room.subs room)
                ]

        Down ->
            Sub.none


type Msg
    = RoomMsg Room.Msg
    | OpenChat


new : Chat
new =
    Chat Down



-- remotemap ( remotestate, remotemsg ) =
--     ( { art
--         | state = Viewing remotestate
--       }
--     , Cmd.map RemoteMsg remotemsg
--     )
-- RemoteMsg remotemsg ->
--     remotemap (Remote.update remotemsg )


update : Msg -> Chat -> ( Chat, Cmd Msg )
update msg chat =
    case msg of
        RoomMsg roommsg ->
            case chat.connection of
                UpIn room ->
                    let
                        roommap ( room, roommsg ) =
                            ( { chat | connection = UpIn room }
                            , Cmd.map RoomMsg roommsg
                            )
                    in
                        roommap (Room.update roommsg room)

                Down ->
                    Debug.crash "Oh noes, room message while no rooms"

        OpenChat ->
            ( { chat | connection = UpIn Room.new }, Cmd.none )


view : Chat -> Html Msg
view chat =
    case chat.connection of
        UpIn room ->
            div [ class "chat" ]
                [ Html.map RoomMsg (Room.view room)
                ]

        Down ->
            button [ onClick OpenChat, autofocus True ] [ text "Open chat" ]
