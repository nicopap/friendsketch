module API
    exposing
        ( exitToJoin
        , Game(..)
        , exitToGame
        , RoomID
        , validRoomID
        , showRoomID
        , Name
        , validName
        , showName
        , roomsCreateRequest
        , roomsJoinRequest
        , wschat
        , LobbyState
        , RoundState
        , ScoresState
        , GameState(..)
        , InfoMsg(..)
        , InfoRequest(..)
        , wsinfoListen
        , wsinfoSend
        , CanvasMsg(..)
        , wscanvasListen
        , wscanvasSend
        )

{-| Exposes the necessary functions and types to access the backend API.

All the types that the API specifies are defined and exported in this
module.

-}

import Color exposing (Color)
import ColorMath exposing (hexToColor, colorToHex)
import ElementRelativeMouseEvents exposing (Point)
import Json.Encode as Enc exposing (Value)
import Json.Decode as Dec exposing (Decoder)
import TypeDecoders exposing (..)
import Http exposing (encodeUri)
import Regex as Re
import Maybe
import WebSocket
import Ports


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


exitToJoin : Name -> Cmd msg
exitToJoin (Name_ username) =
    Ports.stashAndOpen
        ( [("username", username)]
        , "/lobby/join/index.html"
        )


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


exitToGame : Game -> RoomID -> Name -> Cmd msg
exitToGame game (RoomID_ roomid) (Name_ username) =
    Ports.stashAndOpen
        ( [("roomid", roomid), ("username", username)]
        , "/games" +/+ showGame game
        )



--- WebSocket API ---


{-| Generic way of accessing (sending to) a game websocket.
-}
wssend :
    String -> (inmsg -> Value)
    -> Game -> RoomID -> Name
    -> inmsg -> Cmd msg
wssend channel encoder game (RoomID_ roomid) (Name_ name) =
    let
        address =
            "ws://localhost:8080/ws/games" +/+ showGame game +/+ roomid +/+ channel +/+ name
    in
        WebSocket.send address << Enc.encode 0 << encoder


{-| Generic way of accessing (listening to) a game websocket.
-}
wslisten :
    String -> Decoder inmsg
    -> Game -> RoomID -> Name
    -> Maybe (Result String inmsg -> outmsg)
    -> Sub outmsg
wslisten channel decoder game (RoomID_ roomid) (Name_ name) continuation =
    let
        address =
            "ws://localhost:8080/ws/games" +/+ showGame game +/+ roomid +/+ channel +/+ name
    in
        case continuation of
            Nothing ->
                WebSocket.keepAlive address

            Just continuation_ ->
                WebSocket.listen address <|
                    continuation_ << Dec.decodeString decoder


wschat : Game -> RoomID -> Name -> String
wschat game (RoomID_ roomid) (Name_ name) =
    "ws://localhost:8080/ws/games" +/+ showGame game +/+ roomid +/+ "chat" +/+ name


wscanvasListen :
    Game -> RoomID -> Name
    -> Maybe (Result String CanvasMsg -> msg)
    -> Sub msg
wscanvasListen =
    wslisten "canvas" decoderCanvasMsg


wscanvasSend : Game -> RoomID -> Name -> CanvasMsg -> Cmd msg
wscanvasSend =
    wssend "canvas" encoderCanvasMsg


wsinfoListen :
    Game -> RoomID -> Name
    -> Maybe (Result String InfoMsg -> msg)
    -> Sub msg
wsinfoListen =
    wslisten "info" decoderInfo


wsinfoSend : Game -> RoomID -> Name -> InfoRequest -> Cmd msg
wsinfoSend =
    wssend "info" encoderInfoRequest



--- ScoresState ---


type alias ScoresState =
    { scores : List (Name, Int)
    }


decoderScoresState : Decoder ScoresState
decoderScoresState =
    ScoresState
        <*| "scores" :* Dec.list ((,) <*| 0 :^ decoderName |*| 1 :^ Dec.int)



--- LobbyState ---


type alias LobbyState =
    { opponents : List Name
    , master : Bool
    }


decoderLobbyState : Decoder LobbyState
decoderLobbyState =
    LobbyState
        <*| "opponents" :* Dec.list decoderName
        |*| "master" :* Dec.bool



--- RoundState ---


type alias RoundState =
    { spectators : List Name
    , artist : Name
    , timeout : Int
    }


decoderRoundState : Decoder RoundState
decoderRoundState =
    RoundState
        <*| "spectators" :* Dec.list decoderName
        |*| "artist" :* decoderName
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



--- InfoRequest ---


type InfoRequest
    = ReqSync
    | ReqStart


encoderInfoRequest : InfoRequest -> Value
encoderInfoRequest infoRequest =
    let
        type_ =
            case infoRequest of
                ReqSync -> "sync"
                ReqStart -> "start"
    in
        Enc.object [ ( type_, Enc.list [] ) ]



--- CanvasMsg ---


type CanvasMsg
    = CnvStart Point Color Float
    | CnvContinue Point
    | CnvEnd


decoderCanvasMsg : Decoder CanvasMsg
decoderCanvasMsg =
    let
        decoderPoint =
            Point <*| 0 :^ Dec.float |*| 1 :^ Dec.float

        decoderColor =
            Dec.map
                (Result.withDefault Color.black << hexToColor)
                Dec.string
    in
        sumType
            <+| "start" := CnvStart
                <*| 0 :^ decoderPoint
                |*| 1 :^ decoderColor
                |*| 2 :^ Dec.float
            |+| "continue" := CnvContinue <*| decoderPoint
            |+< "end" :- CnvEnd


encoderCanvasMsg : CanvasMsg -> Value
encoderCanvasMsg msg =
    let
        encoderPoint { x, y } =
            Enc.list [ Enc.float x, Enc.float y ]

        encoderColor =
            Enc.string << colorToHex
    in
        case msg of
            CnvStart point color size ->
                Enc.object
                    [ ( "start"
                      , Enc.list
                          [ encoderPoint point
                          , encoderColor color
                          , Enc.float size
                          ]
                      )
                    ]

            CnvContinue point ->
                Enc.object [ ( "continue", encoderPoint point ) ]

            CnvEnd ->
                Enc.object [ ( "end", Enc.list [] ) ]
