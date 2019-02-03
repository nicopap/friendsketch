module Main exposing (main)

{-| The top level logic for the Pintclone game. It's router that interprets
room websocket messages and apply changes to the state of modules
accordingly.
-}

import Maybe
import Debug
import Process
import Task
import Time exposing (second)

import Html as H exposing (Html, div, p, b, h1, h3, text, pre, input)
import Html.Attributes as HA exposing (id, class, href)

import Game exposing (Game)
import Api exposing (GameMsg)


type RetryStatus
    = OneAttempt
    | Attempt Int
    | GivenUp Int
    | NoAttempt

type GameLobby
    = Uninit
    | Running Game
    | Error { title : String, message : String, retry : RetryStatus }


type alias Pintclone =
    { state : GameLobby
    , roomid : Api.RoomID
    , username : Api.Name
    , wslisten : (Result Api.ListenError GameMsg -> Msg) -> Sub Msg
    , wssend : Api.GameReq -> Cmd Msg
    , syncRetries : Int
    , openGameRetries : Int
    }


type alias Flags =
    { roomid : String
    , username : String
    , retries : Int
    }


type Msg
    = GameMsg Game.Msg
    | ListenError Api.ListenError
    | Sync Api.GameState
    | Dummy
    | Reopen



main : Program Flags Pintclone Msg
main =
    H.programWithFlags
        { init = new << sanitizeFlags
        , update = update
        , view = view
        , subscriptions = subs
        }


{-| Crash if the flags are invalid (because it becomes impossible to
construct a logical program at this point).
-}
sanitizeFlags : Flags -> ( Api.RoomID, Api.Name, Int )
sanitizeFlags { roomid, username, retries } =
    let
        maybeSanitized =
            Maybe.map3 (,,)
                (Api.validRoomID roomid)
                (Api.validName username)
                (Just retries)

        crashMessage =
            """
            Game initialization failed, please
            check if your browser supports sessionStorage:

                http://caniuse.com/#search=sessionStorage

            If not, please come back with a compatible browser :)"""
    in
        case maybeSanitized of
            Nothing -> Debug.crash crashMessage
            Just ret -> ret


new : (Api.RoomID, Api.Name, Int) -> ( Pintclone, Cmd Msg )
new (roomid, username, openGameRetries) =
    ( { state = Uninit
      , roomid = roomid
      , username = username
      , wslisten = Api.wsListen roomid username
      , wssend = Api.wsSend roomid username
      , syncRetries = 0
      , openGameRetries = openGameRetries
      }
    , Api.wsSend roomid username Api.ReqSync
    )


update : Msg -> Pintclone -> ( Pintclone, Cmd Msg )
update msg ({ roomid, username, openGameRetries, syncRetries, wssend } as pintclone_) =
    let
        pintclone =
            { pintclone_ | syncRetries = 0 }

        report errorMsg =
            let fullMsg =
                    "Failure in '" ++ Api.showRoomID roomid ++ "' for '"
                    ++ Api.showName username ++ "' with the following error: "
                    ++ errorMsg
            in  Api.reportError fullMsg |> Cmd.map (\() -> Dummy)

        toError title message retry =
            {message=message, retry=retry, title=title}
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
                    updateError <| toError
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
                            Error <| toError
                                "Communication error (reconnecting)"
                                "Websocket connection error"
                                attempts
                    in
                        ( { pintclone | state = newError }
                        , Process.sleep (2.4 * second)
                            |> Task.perform (always Reopen)
                        )

            ListenError (Api.DecodeError msg) ->
                updateError <| toError "Communication error" msg NoAttempt

            Dummy ->
                ( pintclone, Cmd.none )

            Reopen ->
                ( pintclone
                , Api.exitToGame
                    Api.Pintclone roomid username (openGameRetries + 1)
                )

            Sync state ->
                let game = Game.sync state username
                in ( { pintclone | state = Running game }, Cmd.none )


            GameMsg gameMsg ->
                case pintclone.state of
                    Running game ->
                        withGameUpdate <| Game.update gameMsg game

                    anyelse ->
                        if syncRetries >= 3 then
                            updateError <| toError "Synchronisation error" "" NoAttempt
                        else
                            Debug.log
                                ("Inconsistency:" ++ toString anyelse)
                                ( { pintclone | syncRetries = syncRetries + 1 }
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
            Uninit  -> wslisten waitSync
            Error _ -> Sub.none


view : Pintclone -> Html Msg
view pintclone =
    case pintclone.state of
        Uninit ->
            h1 [] [ text "Opening game" ]

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
                secondParagraph = "We are sorry for the inconvenience this causes you."
                thirdParagraph = "A copy of the following error message has already been sent to the developers. We will take care that you won't experience this in the future."
            in
                div []
                    [ h1 [] [ text title ]
                    , p [] [ text firstParagraph ]
                    , H.a
                        [ href "/friendk/lobby/index.html"]
                        [ H.button [] [ text "Join a different game" ] ]
                    , p [] [ text secondParagraph ]
                    , p [] [ text thirdParagraph ]
                    , pre [] [ text message ]
                    ]
