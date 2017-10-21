module API
    exposing
        ( lobbyJoin
        , Game(..)
        , RoomID
        , Name
        , validName
        , showName
        , validRoomID
        , showRoomID
        , gamepage
        , roomsCreateRequest
        , roomsJoinRequest
        , wsinfo
        , wschat
        , wscanvas
        , InfoMsg(..)
        )

{-| Exposes the necessary functions and types to access the backend API.
TODO: Decide what to include in this module.
-}

import Json.Encode as Enc
import Json.Decode as Dec exposing (Decoder)
import TypeDecoders exposing (..)
import Http exposing (encodeUri)
import Regex as Re
import Maybe
import WebSocket


type Game
    = Pintclone

decoderGame : Decoder Game
decoderGame =
    Dec.succeed Pintclone


type Name
    = Name_ String


type RoomID
    = RoomID_ String


(+/+) : String -> String -> String
(+/+) head tail =
    head ++ "/" ++ tail


{-| Turns a maybe type decoder into a type decoder that may fail.
-}
decodeMaybe : String -> Decoder (Maybe a) -> Decoder a
decodeMaybe errorMessage =
    Dec.andThen
        (\x ->
            case x of
                Just val ->
                    Dec.succeed val

                Nothing ->
                    Dec.fail errorMessage
        )


regexValidate : String -> String -> Maybe String
regexValidate re string =
    if Re.contains (Re.regex re) string then
        Just string
    else
        Nothing


{-| Skips the validation part. You should expect the server to send
valid data.
-}
decoderName : Decoder Name
decoderName =
    Dec.map Name_ Dec.string


showName : Name -> String
showName (Name_ stringrep) =
    stringrep


validName : String -> Maybe Name
validName =
    let
        nameRegex =
            "^[!-~\\u00A1-\\u02AF\\u0390-\\u04FF][ -~\\u00A1-\\u02AF\\u0390-\\u04FF]{0,50}[!-~\\u00A1-\\u02AF\\u0390-\\u04FF]$"
    in
        Maybe.map Name_ << regexValidate nameRegex


showRoomID : RoomID -> String
showRoomID (RoomID_ stringrep) =
    stringrep


validRoomID : String -> Maybe RoomID
validRoomID =
    Maybe.map RoomID_ << regexValidate "^[a-z0-9]{5}$"


lobbyJoin : String
lobbyJoin =
    "/lobby/join/index.html"


roomsCreate : String
roomsCreate =
    "/rooms/create"


roomsCreateRequest : Game -> Http.Request RoomID
roomsCreateRequest game =
    let
        body =
            Http.jsonBody <|
                Enc.object [ ( "game", Enc.string <| showGame game ) ]

        responseDecoder =
            decodeMaybe "Invalid room id!" <| validRoomID <*| Dec.string
    in
        Http.post roomsCreate body responseDecoder


roomsJoin : String
roomsJoin =
    "/rooms/join"


roomsJoinRequest : RoomID -> Name -> Http.Request Game
roomsJoinRequest (RoomID_ roomid) (Name_ username) =
    let
        body =
            Http.jsonBody <|
                Enc.object
                    [ ( "roomid", Enc.string roomid )
                    , ( "username", Enc.string username )
                    ]
    in
        Http.post roomsJoin body decoderGame


showGame : Game -> String
showGame game =
    case game of
        Pintclone ->
            "pintclone"


gamepage : Game -> String
gamepage game =
    "/games" +/+ showGame game



--- WebSocket API ---


wschat : Game -> RoomID -> Name -> String
wschat game (RoomID_ roomid) (Name_ name) =
    "ws:///ws/games" +/+ showGame game +/+ roomid +/+ "chat" +/+ name


wscanvas : Game -> RoomID -> Name -> String
wscanvas game (RoomID_ roomid) (Name_ name) =
    "ws:///ws/games" +/+ showGame game +/+ roomid +/+ "canvas" +/+ name


wsinfo : Game -> RoomID -> Name -> (Result String InfoMsg -> msg) -> Sub msg
wsinfo game (RoomID_ roomid) (Name_ name) continuation =
    let
        address =
            "ws:///ws/games" +/+ showGame game +/+ roomid +/+ "info" +/+ name
    in
        WebSocket.listen address <|
            continuation << Dec.decodeString decoderInfo



--- ScoresState ---


type alias ScoresState =
    { scores : List (Name, Int)
    , you : Name
    }


decoderScoresState : Decoder ScoresState
decoderScoresState =
    ScoresState
        <*| "scores" :* Dec.list ((,) <*| 0 :^ decoderName |*| 1 :^ Dec.int)
        |*| "you" :* decoderName



--- LobbyState ---


type alias LobbyState =
    { opponents : List Name
    , you : Name
    , master : Bool
    }


decoderLobbyState : Decoder LobbyState
decoderLobbyState =
    LobbyState
        <*| "opponents" :* Dec.list decoderName
        |*| "you" :* decoderName
        |*| "master" :* Dec.bool



--- RoundState ---


type alias RoundState =
    { spectators : List Name
    , artist : Name
    , you : Name
    , timeout : Int
    }


decoderRoundState : Decoder RoundState
decoderRoundState =
    RoundState
        <*| "spectators" :* Dec.list decoderName
        |*| "artist" :* decoderName
        |*| "you" :* decoderName
        |*| "timeout" :* Dec.int



--- GameState ---


type GameState
    = Summary ScoresState
    | Round RoundState
    | Lobby LobbyState


decoderGameState : Decoder GameState
decoderGameState =
    sumType
        <+| "summary" := Summary <*| decoderScoresState
        |+| "round" := Round <*| decoderRoundState
        |+< "lobby" := Lobby <*| decoderLobbyState



--- InfoMsg ---


type InfoMsg
    = Joined Name
    | Left Name
    | Sync GameState
    | Mastery


decoderInfo : Decoder InfoMsg
decoderInfo =
    sumType
        <+| "joined" := Joined <*| decoderName
        |+| "left" := Left <*| decoderName
        |+| "sync" := Sync <*| decoderGameState
        |+< "mastery" :- Mastery

