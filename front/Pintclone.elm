module Main exposing (main)

{-| The top level logic for the Pintclone game. It's router that interprets
room websocket messages and apply changes to the state of modules
accordingly.
-}

import Debug
import Process
import Task
import Time exposing (Time, second)

import Html as H exposing (Html, div, p, b, h1, h3, text, pre, input)
import Html.Attributes as HA exposing (id, class, href)

import Game exposing (Game)
import Api exposing (GameMsg)


type RetryStatus
    = OneAttempt
    | Attempt Int
    | GivenUp Int
    | NoAttempt
    | NoInit
    | TooLong

type GameLobby
    = Uninit
    | LongUninit
    | Running Game
    | Error Error_

type alias Error_ =
    { title : String
    , message : String
    , retry : RetryStatus
    }

type alias Pintclone =
    { state : GameLobby
    , roomid : Api.RoomID
    , username : Api.Name
    , wslisten : (Result Api.ListenError GameMsg -> Msg) -> Sub Msg
    , wssend : Api.GameReq -> Cmd Msg
    , isUnsync : Bool
    , openGameRetries : Int
    }


type Msg
    = GameMsg Game.Msg
    | ListenError Api.ListenError
    | Sync Api.GameState
    | LongInit
    | TooLongInit
    | Dummy
    | Reopen



main : Program Api.InitFlags Pintclone Msg
main =
    H.programWithFlags
        { init = new << Api.extractInitFlags
        , update = update
        , view = view
        , subscriptions = subs
        }



new : Result (String, Api.RoomID, Api.Name) (Api.ConnId, Api.RoomID, Api.Name, Int)
   -> ( Pintclone, Cmd Msg )
new flags =
    case flags of
        Ok (connid, roomid, username, openGameRetries) ->
            ( { state = Uninit
              , roomid = roomid
              , username = username
              , wslisten = Api.wsListen connid
              , wssend = Api.wsSend connid
              , isUnsync = False
              , openGameRetries = openGameRetries
              }
            , Cmd.batch
                [ Api.wsSend connid Api.ReqSync
                , delay (3 * second) LongInit
                ]
            )
        Err (message, fakeRoomid, fakeName) ->
            ( { state = Error <| Error_ "The game Can't start" message NoInit
              , roomid = fakeRoomid
              , username = fakeName
              , wslisten = always Sub.none
              , wssend = always Cmd.none
              , isUnsync = True
              , openGameRetries = 3
              }
            , Api.reportError ("cannot initialize the game:" ++ message)
                |> Cmd.map (\() -> Dummy)
            )

delay : Time -> Msg -> Cmd Msg
delay by msg =
    Process.sleep (2.4 * second) |> Task.perform (always msg)

update : Msg -> Pintclone -> ( Pintclone, Cmd Msg )
update msg ({ roomid, username, openGameRetries, wssend } as pintclone) =
    let
        report errorMsg =
            let fullMsg =
                    "Failure in '" ++ Api.showRoomID roomid ++ "' for '"
                    ++ Api.showName username ++ "' with the following error: "
                    ++ errorMsg
            in  Api.reportError fullMsg |> Cmd.map (\() -> Dummy)

        updateError error =
            ({ pintclone | state = Error error }, report error.message)

        withGameUpdate (game, gameCmd) =
            ( { pintclone | state = Running game }
            , case gameCmd of
                Game.Send req -> wssend req
                Game.Execute cmd -> Cmd.map GameMsg cmd
                Game.SendExecute req cmd ->
                    Cmd.batch [ wssend req , Cmd.map GameMsg cmd ]
            )
    in
        case msg of
            ListenError Api.BadSend ->
                if openGameRetries >= 2 then
                    updateError <|
                        Error_
                            "Communication error"
                            ("Attempted to connect " ++ toString openGameRetries
                            ++ " times without success. Giving up")
                            (GivenUp (openGameRetries + 1))
                else
                    let attempts =
                            if openGameRetries == 0 then
                                OneAttempt
                            else
                                Attempt (openGameRetries + 1)
                        newError =
                            Error <|
                                Error_
                                    "Communication error (reconnecting)"
                                    "Websocket connection error"
                                    attempts
                    in
                        ( { pintclone | state = newError }
                        , delay (2.4 * second) Reopen
                        )

            ListenError (Api.DecodeError msg) ->
                updateError <| Error_ "Communication error" msg NoAttempt

            Dummy ->
                ( pintclone, Cmd.none )

            Reopen ->
                ( pintclone , Api.reopenGame roomid username openGameRetries )

            Sync state ->
                let game = Game.sync state username
                in ( { pintclone | state = Running game }, Cmd.none )

            LongInit ->
                case pintclone.state of
                    Uninit ->
                        ( { pintclone | state = LongUninit }
                        , delay (30 * second) TooLongInit
                        )
                    _ ->
                        ( pintclone, Cmd.none )

            TooLongInit ->
                case pintclone.state of
                    LongUninit ->
                        updateError <|
                            Error_
                                "Communication error"
                                "Websocket connection took too long"
                                TooLong
                    _ ->
                        ( pintclone, Cmd.none )

            GameMsg gameMsg ->
                case pintclone.state of
                    Running game ->
                        withGameUpdate <| Game.update gameMsg game

                    anyelse ->
                        if pintclone.isUnsync then
                            updateError <|
                                Error_ "Synchronisation error" "" NoAttempt
                        else
                            Debug.log
                                ("Inconsistency:" ++ toString anyelse)
                                ( { pintclone | isUnsync = True }
                                , pintclone.wssend Api.ReqSync
                                )


subs : Pintclone -> Sub Msg
subs { wslisten, state } =
    let adaptApi msg =
            case msg of
                Err err -> ListenError err
                Ok ok -> GameMsg <| Game.receive ok
        waitSync msg =
            case msg of
                Err err -> ListenError err
                Ok (Api.HiddenEvent (Api.EhSync gameState)) ->
                    Sync gameState
                Ok _ -> Dummy
    in
        case state of
            Running game -> Sub.batch
                [ Sub.map GameMsg <| Game.subs game
                , wslisten adaptApi
                ]
            Uninit -> wslisten waitSync
            LongUninit -> wslisten waitSync
            Error _ -> Sub.none


view : Pintclone -> Html Msg
view pintclone =
    case pintclone.state of
        Uninit ->
            h1 [] [ text "Opening game" ]

        LongUninit ->
            div []
                [ h1 [] [ text "Opening game" ]
                , p [] [ text "The connection to the server is taking a while..." ]
                , p [] [ text "Maybe try refreshing?" ]
                ]

        Running game ->
            H.map GameMsg <| Game.view pintclone.roomid game

        Error { title, message, retry } ->
            let
                firstParagraph =
                    case retry of
                        GivenUp c -> "After " ++ toString c ++ " tries, we still couldn't connect to the game, your only option is to try to join a different room or change display name."
                        OneAttempt -> "Couldn't connect to the game. Retrying..."
                        Attempt c -> "After " ++ toString c ++ " tries, we still couldn't connect to the game. Retrying..."
                        NoAttempt -> "An irreversible error occured and your only option is to try to join a different room or change display name."
                        NoInit -> "The game can't start without proper initialization: use the room creation page or a join link to join a game. Friendsketch is only accessible through those pages."
                        TooLong -> "We took too long to join the game, the server probably forgot about us by this time."
                secondParagraph = "We are sorry for the inconvenience this causes you."
                thirdParagraph = "A copy of the following error message has already been sent to the developers. We will take care that you won't experience this in the future."
            in
                div []
                    [ h1 [] [ text title ]
                    , p [] [ text firstParagraph ]
                    , H.a
                        [ href "/lobby/index.html"]
                        [ H.button [] [ text "Join a different game" ] ]
                    , p [] [ text secondParagraph ]
                    , p [] [ text thirdParagraph ]
                    , pre [] [ text message ]
                    ]
