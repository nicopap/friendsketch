module Room exposing (Room, canvasUrl, chatUrl, new, findOponent)


type alias Room_ =
    { roomid : String
    , baseurl : String
    , userName : String
    , oponent : Maybe String
    }


type Room
    = Room Room_


chatUrl : Room -> String
chatUrl (Room { roomid, baseurl }) =
    "ws://" ++ baseurl ++ roomid ++ "/chat"


canvasUrl : Room -> String
canvasUrl (Room { roomid, baseurl }) =
    "ws://" ++ baseurl ++ roomid ++ "/canvas"


new : String -> String -> String -> Room
new baseurl roomid userName =
    Room <| Room_ roomid baseurl userName Nothing


findOponent : String -> Room -> Room
findOponent oponentName (Room room) =
    Room { room | oponent = Just oponentName }
