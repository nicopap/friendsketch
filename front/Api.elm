module Api
    exposing
        ( Game(..)
        , reopenGame
        , RoomID
        , showRoomID
        , showRoomLink
        , InitFlags
        , extractInitFlags
        , Name
        , showName
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
        , ConnId
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
import Maybe
import NeatSocket
import Ports


type Game = Pintclone

type ConnId = ConnId_ String
type Name = Name_ String
type ChatContent = ChatContent_ String
type RoomID = RoomID_ String

decoderGame : Decoder Game
decoderGame =
    Dec.succeed Pintclone


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


showRoomID : RoomID -> String
showRoomID (RoomID_ stringrep) =
    stringrep

showRoomLink : RoomID -> String
showRoomLink (RoomID_ roomid) =
    "http://localhost:8080/friendk/join/" ++ roomid

showGame : Game -> String
showGame game =
    case game of
        Pintclone ->
            "classic"


reopenGame : RoomID -> Name -> Int -> Cmd msg
reopenGame (RoomID_ roomid) (Name_ username) retries =
    Ports.stashAndOpen
        ( [ ("roomid", Enc.string roomid)
          , ("username", Enc.string username)
          , ("retries", Enc.int (retries + 1))
          ]
        , "."
        )



type alias InitFlags = Dec.Value

extractInitFlags : Dec.Value -> Result (String, RoomID, Name) (ConnId, RoomID, Name, Int)
extractInitFlags flags =
    let
        decodeFlags =
            (,,,)
                <*| "connection" :* (Dec.map ConnId_ Dec.string)
                |*| "roomid" :* (Dec.map RoomID_ Dec.string)
                |*| "username" :* decoderName
                |*| "retries" :* Dec.int
    in
        Dec.decodeValue decodeFlags flags
            |> Result.mapError (\x -> (toString x, RoomID_ "", Name_ ""))



--- NeatSocket Api ---


{-| Generic way of accessing (sending to) a game websocket.
-}
wsSend : ConnId -> GameReq -> Cmd msg
wsSend (ConnId_ connid) =
    let address = "ws://localhost:8080/friendk/ws" +/+ connid
    in  NeatSocket.send address << Enc.encode 0 << encoderGameReq

type ListenError
    = DecodeError String
    | BadSend

{-| Generic way of accessing (listening to) a game websocket.
-}
wsListen : ConnId -> (Result ListenError GameMsg -> msg) -> Sub msg
wsListen (ConnId_ connid) continuation =
    let address = "ws://localhost:8080/friendk/ws" +/+ connid

        withNiceError serverResponse =
            case serverResponse of
                NeatSocket.Message toDecode ->
                    Dec.decodeString decoderGameMsg toDecode
                        |> Result.mapError (\decodeErr ->
                            "{\"json\":" ++ toString decodeErr ++ ",\"msg\":"++toDecode ++ "}"
                                |> DecodeError
                        )
                NeatSocket.Refused -> Err BadSend
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
