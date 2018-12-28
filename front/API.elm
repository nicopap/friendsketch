module API
    exposing
        ( Game(..)
        , exitToGame
        , RoomID
        , validRoomID
        , showRoomID
        , Name
        , validName
        , showName
        , roomsCreateRequest
        , roomsJoinRequest
        , LobbyState
        , RoundState
        , ScoresState
        , GameState(..)
        , InfoMsg(..)
        , InfoRequest(..)
        , GameMsg(..)
        , GameReq(..)
        , wsListen
        , wsSend
        , CanvasMsg(..)
        )

{-| Exposes the necessary functions and types to access the backend API.

All the types that the API specifies are defined and exported in this
module.

-}

import Color exposing (Color)
import ColorMath exposing (hexToColor, colorToHex)
import ElementRelativeMouseEvents exposing (Point)
import Json.Encode as Enc exposing (Value)
import Json.Decode as Dec exposing (Decoder, oneOf)
import TypeDecoders exposing ((<*|),(|*|),(:=),(:-),(:^), (:*))
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
        (Maybe.map Dec.succeed >> Maybe.withDefault (Dec.fail errorMessage))


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
validName toValidate =
    let
        nameRegex =
            "^[!-~\\u00A1-\\u02AF\\u0390-\\u04FF][ -~\\u00A1-\\u02AF\\u0390-\\u04FF]{0,50}[!-~\\u00A1-\\u02AF\\u0390-\\u04FF]$"
    in
        regexValidate nameRegex toValidate
            |> Maybe.map Name_


showRoomID : RoomID -> String
showRoomID (RoomID_ stringrep) =
    stringrep


validRoomID : String -> Maybe RoomID
validRoomID =
    Maybe.map RoomID_ << regexValidate "^[A-Z][a-z]+\\.[A-Z][a-z]*[A-Z]?[a-z]+$"


roomsCreate : String
roomsCreate =
    "/rooms/create"


roomsCreateRequest : Game -> Name -> Http.Request RoomID
roomsCreateRequest game (Name_ name) =
    let
        body =
            Http.jsonBody <|
                Enc.object
                    [ ( "game", Enc.string <| showGame game )
                    , ( "username", Enc.string name )
                    ]

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
        , "/games" +/+ showGame game +/+ "index.html"
        )



--- WebSocket API ---


{-| Generic way of accessing (sending to) a game websocket.
-}
wsSend : RoomID -> Name -> GameReq -> Cmd msg
wsSend (RoomID_ roomid) (Name_ name) =
    let
        address =
            "ws://localhost:8080/ws" +/+ roomid +/+ name
    in
        WebSocket.send address << Enc.encode 0 << encoderGameReq


{-| Generic way of accessing (listening to) a game websocket.
-}
wsListen : RoomID -> Name -> Maybe (Result String GameMsg -> msg) -> Sub msg
wsListen (RoomID_ roomid) (Name_ name) continuation =
    let
        address =
            "ws://localhost:8080/ws" +/+ roomid +/+ name

        decodeWithNiceError toDecode =
            Dec.decodeString decoderGameMsg toDecode
                |> Result.mapError (\decodeErr ->
                    "{\"json\":"
                        ++ toString decodeErr
                        ++ ",\"msg\":"++toDecode
                        ++ "}"
                )
    in
        case continuation of
            Nothing ->
                WebSocket.keepAlive address

            Just continuation_ ->
                WebSocket.listen address <|
                    continuation_ << decodeWithNiceError


--- RoundSummary ---


type alias Score = Int


type RoundSummary
    = WasArtist Score
    | HasGuessed Score
    | HasFailed
    | WasAbsent


decoderRoundSummary : Decoder RoundSummary
decoderRoundSummary =
    oneOf
        [ "artist" := WasArtist <*| Dec.int
        , "guessed" := HasGuessed <*| Dec.int
        , "failed" :- HasFailed
        , "absent" :- WasAbsent
        ]


decoderScore : Decoder (List (Name, List RoundSummary))
decoderScore =
    Dec.list ((,)
        <*| 0 :^ decoderName
        |*| 1 :^ Dec.list decoderRoundSummary
        )



--- ScoresState ---


type alias ScoresState =
    { scores : List (Name, List RoundSummary)
    }


decoderScoresState : Decoder ScoresState
decoderScoresState =
    ScoresState
        <*| "scores" :* decoderScore



--- LobbyState ---


type alias LobbyState =
    { players : List Name
    , master : Bool
    }


decoderLobbyState : Decoder LobbyState
decoderLobbyState =
    LobbyState
        <*| "players" :* Dec.list decoderName
        |*| "master" :* Dec.bool



--- RoundState ---


type alias RoundState =
    { playerScores : List (Name, List RoundSummary)
    , artist : Name
    , timeout : Int
    }


decoderRoundState : Decoder RoundState
decoderRoundState =
    RoundState
        <*| "players" :* decoderScore
        |*| "artist" :* decoderName
        |*| "timeout" :* Dec.int



--- GameState ---


type GameState
    = Summary ScoresState
    | Round RoundState
    | Lobby LobbyState


decoderGameState : Decoder GameState
decoderGameState =
    oneOf
        [ "summary" := Summary <*| decoderScoresState
        , "round" := Round <*| decoderRoundState
        , "lobby" := Lobby <*| decoderLobbyState
        ]



--- InfoMsg ---


type InfoMsg
    = Joined Name
    | Left Name
    | Sync GameState
    | Mastery


decoderInfo : Decoder InfoMsg
decoderInfo =
    oneOf
        [ "joined" := Joined <*| decoderName
        , "left" := Left <*| decoderName
        , "sync" := Sync <*| decoderGameState
        , "mastery" :- Mastery
        ]



--- InfoRequest ---


type InfoRequest
    = ReqSync
    | ReqStart
    | ReqWarn String


encoderInfoRequest : InfoRequest -> Value
encoderInfoRequest infoRequest =
    case infoRequest of
        ReqSync -> Enc.string "sync"
        ReqStart -> Enc.string "start"
        ReqWarn msg -> Enc.object [("warn", Enc.string msg)]


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
        oneOf
            [ "start" := CnvStart
                <*| 0 :^ decoderPoint
                |*| 1 :^ decoderColor
                |*| 2 :^ Dec.float
            , "continue" := CnvContinue <*| decoderPoint
            , "end" :- CnvEnd
            ]


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
                Enc.string "end"

type GameMsg
    = CanvasMsg CanvasMsg
    | InfoMsg InfoMsg

type GameReq
    = CanvasReq CanvasMsg
    | InfoReq InfoRequest


decoderGameMsg : Decoder GameMsg
decoderGameMsg =
    oneOf
        [ "canvas" := CanvasMsg <*| decoderCanvasMsg
        , "info" := InfoMsg <*| decoderInfo
        ]

encoderGameReq : GameReq -> Value
encoderGameReq req =
    case req of
        CanvasReq req_ ->
            Enc.object [ ("canvas", encoderCanvasMsg req_)]

        InfoReq req_ ->
            Enc.object [ ("info", encoderInfoRequest req_)]
