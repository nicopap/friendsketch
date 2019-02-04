module Api
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
        , reportError
        , RoundState
        , Scoreboard
        , Stroke
        , Drawing
        , GameState
        , GameScreen(..)
        , GameMsg(..)
        , GameReq(..)
        , wsListen
        , wsSend
        , CanvasMsg(..)
        , ListenError(DecodeError, BadSend)
        , ChatMsg
        , ChatContent
        , validChatContent
        , showChatContent
        , Guess(..)
        , RoundStart
        , RoundScore(..)
        , VisibleEvent(..)
        , HiddenEvent(..)
        )

{-| Exposes the necessary functions and types to access the backend Api.

All the types that the Api specifies are defined and exported in this
module.

-}

import List.Nonempty as NE
import Color exposing (Color)
import ColorMath exposing (hexToColor, colorToHex)
import ElementRelativeMouseEvents exposing (Point)
import Json.Encode as Enc exposing (Value)
import Json.Decode as Dec exposing (Decoder, oneOf)
import TypeDecoders exposing ((<*|),(|*|),(:=),(:-),(:^), (:*))
import Http exposing (encodeUri)
import Task exposing (Task)
import Regex as Re
import Maybe
import NeatSocket
import Ports


type Game
    = Pintclone

decoderGame : Decoder Game
decoderGame =
    Dec.succeed Pintclone


type Name
    = Name_ String

type ChatContent
    = ChatContent_ String

type RoomID
    = RoomID_ String


(+/+) : String -> String -> String
(+/+) head tail =
    head ++ "/" ++ tail

{-| Report an error to the game server.
-}
reportError : String -> Cmd ()
reportError error_msg =
    let
        request =
            Http.post
                "/friendk/report"
                (Http.stringBody "plain/text" error_msg)
                (Dec.succeed ())
    in
        Http.toTask request
            |> Task.attempt (\_ -> ())


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

validChatContent : String -> Maybe ChatContent
validChatContent toValidate =
    if String.length toValidate < 300 && not (String.isEmpty toValidate) then
        Just <| ChatContent_ toValidate
    else
        Nothing


showChatContent : ChatContent -> String
showChatContent (ChatContent_ stringrep) =
    stringrep

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
    Maybe.map RoomID_ << regexValidate "^[A-Z][a-z]+-[A-Z][a-z]*[A-Z]?[a-z]+$"


roomsCreate : String
roomsCreate =
    "/friendk/rooms/create"


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
    "/friendk/rooms/join"


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
            "classic"


exitToGame : Game -> RoomID -> Name -> Int -> Cmd msg
exitToGame game (RoomID_ roomid) (Name_ username) retries =
    Ports.stashAndOpen
        ( [ ("roomid", Enc.string roomid)
          , ("username", Enc.string username)
          , ("retries", Enc.int retries)
          ]
        , "/friendk/games" +/+ showGame game +/+ "index.html"
        )



--- NeatSocket Api ---


{-| Generic way of accessing (sending to) a game websocket.
-}
wsSend : RoomID -> Name -> GameReq -> Cmd msg
wsSend (RoomID_ roomid) (Name_ name) =
    let
        address =
            "ws://localhost:8080/friendk/ws" +/+ roomid +/+ name
    in
        NeatSocket.send address << Enc.encode 0 << encoderGameReq

type ListenError
    = DecodeError String
    | BadSend

{-| Generic way of accessing (listening to) a game websocket.
-}
wsListen : RoomID -> Name -> (Result ListenError GameMsg -> msg) -> Sub msg
wsListen (RoomID_ roomid) (Name_ name) continuation =
    let
        address =
            "ws://localhost:8080/friendk/ws" +/+ roomid +/+ name

        withNiceError serverResponse =
            case serverResponse of
                NeatSocket.Message toDecode ->
                    Dec.decodeString decoderGameMsg toDecode
                        |> Result.mapError (\decodeErr ->
                            "{\"json\":" ++ toString decodeErr ++ ",\"msg\":"++toDecode ++ "}"
                                |> DecodeError
                        )

                NeatSocket.Refused ->
                    Err BadSend
    in
        NeatSocket.listen address (continuation << withNiceError)


--- RoundScore ---


type alias Score = Int


type RoundScore
    = WasArtist Score
    | HasGuessed Score
    | HasFailed
    | WasAbsent


decoderRoundScore : Decoder RoundScore
decoderRoundScore =
    oneOf
        [ "artist" := WasArtist <*| Dec.int
        , "guessed" := HasGuessed <*| Dec.int
        , "failed" :- HasFailed
        , "absent" :- WasAbsent
        ]

type alias Scoreboard = List (Name, List RoundScore)

decoderScoreboard : Decoder Scoreboard
decoderScoreboard =
    Dec.list ((,)
        <*| 0 :^ decoderName
        |*| 1 :^ Dec.list decoderRoundScore
    )

decoderScore : Decoder (List (Name, RoundScore))
decoderScore =
    Dec.list ((,)
        <*| 0 :^ decoderName
        |*| 1 :^ decoderRoundScore
    )



--- RoundState ---


type alias RoundState =
    { drawing : List Stroke
    , artist : Name
    , timeout : Int
    }


decoderRoundState : Decoder RoundState
decoderRoundState =
    RoundState
        <*| "drawing" :* decoderDrawing
        |*| "artist" :* decoderName
        |*| "timeout" :* Dec.int



--- CanvasState ---


type alias Stroke =
    { points : NE.Nonempty Point
    , color : Color
    , size : Float
    }

decoderStroke : Decoder Stroke
decoderStroke =
    let
        toStroke : Point -> List Point -> Color -> Float -> Stroke
        toStroke head tail =
            Stroke (NE.Nonempty head tail)
    in
        toStroke
            <*| 0 :^ decoderPoint
            |*| 1 :^ Dec.list decoderPoint
            |*| 2 :^ decoderColor
            |*| 3 :^ Dec.float

type alias Drawing = List Stroke

decoderDrawing : Decoder Drawing
decoderDrawing =
    Dec.list decoderStroke

--- GameState ---

type alias GameState =
    { scores : Scoreboard
    , screen : GameScreen
    , history : List VisibleEvent
    }

type GameScreen
    = Summary
    | RoundScores
    | Round RoundState
    | Lobby Name


decoderGameState : Decoder GameState
decoderGameState =
    GameState
        <*| "scores" :* decoderScoreboard
        |*| "screen" :* decoderGameScreen
        |*| "history" :* Dec.list decoderVisibleEvent


decoderGameScreen : Decoder GameScreen
decoderGameScreen =
    oneOf
        [ "endsummary" :- Summary
        , "scores" :- RoundScores
        , "round" := Round <*| decoderRoundState
        , "lobby" := Lobby <*| "master" :* decoderName
        ]



--- CanvasMsg ---


type CanvasMsg
    = CnvStart Point Color Float
    | CnvContinue Point
    | CnvEnd

decoderPoint : Decoder Point
decoderPoint =
    Point <*| 0 :^ Dec.float |*| 1 :^ Dec.float

decoderColor : Decoder Color
decoderColor =
    Dec.map
        (Result.withDefault Color.black << hexToColor)
        Dec.string

decoderCanvasMsg : Decoder CanvasMsg
decoderCanvasMsg =
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
    | VisibleEvent VisibleEvent
    | HiddenEvent HiddenEvent



type alias ChatMsg =
        { content : ChatContent
        , author : Name
        }

decoderChatMsg : Decoder ChatMsg
decoderChatMsg =
    ChatMsg
        <*| "content" :* Dec.map ChatContent_ Dec.string
        |*| "author" :* Dec.map Name_ Dec.string

type GameReq
    = CanvasReq CanvasMsg
    | ChatReq ChatContent
    | ReqStart
    | ReqSync


decoderGameMsg : Decoder GameMsg
decoderGameMsg =
    oneOf
        [ "canvas" := CanvasMsg <*| decoderCanvasMsg
        , "event" := oneOf
            [ Dec.map VisibleEvent decoderVisibleEvent
            , Dec.map HiddenEvent decoderHiddenEvent
            ]
        ]

encoderGameReq : GameReq -> Value
encoderGameReq req =
    case req of
        CanvasReq req_ ->
            Enc.object [ ("canvas", encoderCanvasMsg req_)]

        ChatReq (ChatContent_ req_) ->
            Enc.object [ ("chat", Enc.string req_)]

        ReqSync -> Enc.string "sync"
        ReqStart -> Enc.string "start"



---   Events / Guess   ---



type Guess
    = ForArtist String
    | GuessOfLength Int

type alias RoundStart =
    { timeout : Int
    , artist : Name
    , word : Guess
    }

decoderRoundStart : Decoder RoundStart
decoderRoundStart =
    RoundStart
        <*| "timeout" :* Dec.int
        |*| "artist" :* decoderName
        |*| "word" :* decoderGuess

decoderGuess : Decoder Guess
decoderGuess =
    oneOf
        [ "artist" := ForArtist <*| Dec.string
        , "guess" := GuessOfLength <*| Dec.int
        ]

type VisibleEvent
    = EvGuessed Name
    | EvLeft Name
    | EvJoined Name
    | EvMessage ChatMsg
    | EvStart Name
    | EvOver String


type HiddenEvent
    = EhCorrect String
    | EhTimeout Int
    | EhSync GameState
    | EhMastery
    | EhOver String (List (Name, RoundScore))
    | EhStart RoundStart
    | EhReveal Int Char


decoderVisibleEvent : Decoder VisibleEvent
decoderVisibleEvent =
    oneOf
        [ "guessed" := EvGuessed <*| decoderName
        , "left" := EvLeft <*| decoderName
        , "joined" := EvJoined <*| decoderName
        , "message" := EvMessage <*| decoderChatMsg
        , "syncstart" := EvStart <*| decoderName
        , "syncover" := EvOver <*| Dec.string
        ]

decoderHiddenEvent : Decoder HiddenEvent
decoderHiddenEvent =
    let
        decoderChar string =
            case String.toList string of
                char::[] -> Dec.succeed char
                _ -> Dec.fail "Character is not a character"
    in
        oneOf
            [ "correct" := EhCorrect <*| Dec.string
            , "timeoutsync" := EhTimeout <*| Dec.int
            , "sync" := EhSync <*| decoderGameState
            , "mastery" :- EhMastery
            , "over" := EhOver
                <*| 0 :^ Dec.string
                |*| 1 :^ decoderScore
            , "start" := EhStart <*| decoderRoundStart
            , "reveal" := EhReveal
                <*| 0 :^ Dec.int
                |*| 1 :^ (Dec.andThen decoderChar Dec.string)
            ]
