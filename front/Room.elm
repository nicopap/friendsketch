module Room exposing (Room, canvasUrl, chatUrl, infoUrl, new, join, infoSub, view)

import WebSocket
import Html as H exposing (Html)


-- TODO: Create local state (ie: user list)
-- & view & update & caetera


type alias Room_ =
    { roomid : String
    , baseurl : String
    , userName : String
    , oponents : List String
    }


type Room
    = Room Room_


chatUrl : Room -> String
chatUrl (Room { roomid, baseurl }) =
    "ws://" ++ baseurl ++ roomid ++ "/chat"


canvasUrl : Room -> String
canvasUrl (Room { roomid, baseurl }) =
    "ws://" ++ baseurl ++ roomid ++ "/canvas"


infoUrl : Room -> String
infoUrl (Room { roomid, baseurl, userName }) =
    "ws://" ++ baseurl ++ roomid ++ "/info/" ++ userName


new : String -> String -> String -> Room
new baseurl roomid userName =
    Room
        { roomid = roomid
        , baseurl = baseurl
        , userName = userName
        , oponents = []
        }


{-| The URL to request the websocket to connect to roomId.
-}
join : String -> String
join roomid =
    "/room/join/" ++ roomid


infoSub : (String -> msg) -> Room -> Sub msg
infoSub continue room =
    WebSocket.listen (infoUrl room) continue


{-| A list of connected users to the room
-}
view : Room -> Html msg
view _ =
    H.text "Room list view"
