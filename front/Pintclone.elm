module Main exposing (main)

{-| The top level logic for the Pintclone game. It's router that interprets
room websocket messages and apply changes to the state of modules
accordingly.
-}

import Tuple exposing (mapSecond, mapFirst)
import Maybe
import Debug
import Html as H exposing (Html, div, p, b, h1, h3, text, pre, input)
import Html.Attributes as HA exposing (id, class, href)
import Html.Events as HE
import Pintclone.Room as Room exposing (Room)
import Pintclone.Guess as Guess exposing (Guess)
import Canvas exposing (Canvas)
import Chat exposing (Chat)
import Process
import Task
import Time exposing (second)
import Ports
import Api exposing (GameReq(InfoReq, CanvasReq), GameMsg)


type alias RoundState_T =
    { room : Room
    , canvas : Canvas
    , guess : Guess
    }

type alias LobbyState_T =
    { room : Room
    , canvas : Canvas
    , hideId : Bool
    }

type RetryStatus
    = OneAttempt
    | Attempt Int
    | GivenUp Int
    | NoAttempt

type GameLobby
    = Uninit
    | LobbyState LobbyState_T
    | RoundState RoundState_T
    | FinalState { room : Room, scores : List ( Api.Name, Int ) }
    | ErrorState { title : String, message : String, retry : RetryStatus }


type alias Pintclone =
    { state : GameLobby
    , roomid : Api.RoomID
    , username : Api.Name
    , wslisten : Maybe (Result Api.ListenError GameMsg -> Msg) -> Sub Msg
    , wssend : Api.GameReq -> Cmd Msg
    , syncRetries : Int
    , openGameRetries : Int
    , chat : Chat
    }


type alias Flags =
    { roomid : String
    , username : String
    , retries : Int
    }


type LobbyMsg
    = TogglePassView
    | StartGame


type Msg
    = Info Api.InfoMsg
    | Classic Api.ClassicMsg
    | ChatMsg Chat.Msg
    | LobbyMsg LobbyMsg
    | CanvasMsg Canvas.Msg
    | ListenError Api.ListenError
    | Dummy
    | Reopen



main : Program Flags Pintclone Msg
main =
    H.programWithFlags
        { init = new << sanitizeFlags
        , update = update
        , view = div [] << view
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
            Nothing ->
                Debug.crash crashMessage

            Just ret ->
                ret


new : (Api.RoomID, Api.Name, Int) -> ( Pintclone, Cmd Msg )
new (roomid, username, openGameRetries) =
    ( { state = Uninit
      , roomid = roomid
      , username = username
      , wslisten = Api.wsListen roomid username
      , wssend = Api.wsSend roomid username
      , syncRetries = 0
      , openGameRetries = openGameRetries
      , chat = Chat.new username []
      }
    , Api.wsSend roomid username (InfoReq Api.ReqSync)
    )


newScores : Api.ScoresState -> Pintclone -> Pintclone
newScores _ pintclone =
    { pintclone | state = Uninit }


{-| Start at the Pregame mode.
-}
fromLobby : Api.LobbyState -> Pintclone -> Pintclone
fromLobby { players, master } ({ roomid, username } as pintclone) =
    let
        status =
            if master == username then
                Room.Master
            else
                Room.Peasant

        newState =
            LobbyState
                { room = Room.newInLobby status username players
                , canvas = Canvas.new [] Canvas.Pregame
                , hideId = True
                }
    in
        { pintclone | state = newState }


{-| Start a new round of Pintclone.
-}
joinRound : Api.Drawing -> Api.RoundState -> Pintclone -> Pintclone
joinRound drawing { playerScores, artist, timeout } ({ username } as pintclone) =
    let
        scores =
            List.map Tuple.first playerScores

        newInnerState room canvasState =
            { room = Room.joinRound username scores room
            , canvas = Canvas.new drawing canvasState
            , guess = Guess.new Nothing timeout
            }

        newGameState =
            if username == artist then
                newInnerState Room.Me Canvas.Artist
            else
                newInnerState (Room.Another artist) Canvas.Spectator
    in
        { pintclone | state = RoundState newGameState }


updateCanvas : Canvas.Msg -> GameLobby -> ( GameLobby, Maybe Api.CanvasMsg )
updateCanvas msg state =
    let
        newState constr state_ =
            let
                ( newCanvas, response ) =
                    Canvas.update msg state_.canvas
            in
                ( constr { state_ | canvas = newCanvas }, response )
    in
        case state of
            LobbyState state_ ->
                newState LobbyState state_

            RoundState state_ ->
                newState RoundState state_

            anyelse ->
                ( anyelse, Nothing )


stateByRoomUpdate : GameLobby ->  (Room -> Room) -> Cmd m -> ( GameLobby, Cmd m )
stateByRoomUpdate state change ifUninit =
    let
        mod f state_ newRoom =
            if Room.alone newRoom then
                let newLobby =
                        LobbyState
                            { room = newRoom
                            , canvas = Canvas.new [] Canvas.Pregame
                            , hideId = True
                            }
                in  ( newLobby , Cmd.none )
            else
                ( f { state_ | room = newRoom } , Cmd.none )
    in
        case state of
            Uninit ->
                ( state , ifUninit )

            LobbyState ({ room } as state_) ->
                mod LobbyState state_ (change room)

            RoundState ({ room } as state_) ->
                mod RoundState state_ (change room)

            FinalState ({ room } as state_) ->
                mod FinalState state_ (change room)

            ErrorState _ ->
                ( state, Cmd.none )


updateInfo : Api.InfoMsg -> Pintclone -> ( Pintclone, Cmd Msg )
updateInfo msg ({ username, state, wssend, chat } as pintclone) =
    let
        tupleAndThen : (a -> (b, Cmd msg)) -> (a, Cmd msg) -> (b, Cmd msg)
        tupleAndThen f (a, orig_cmd) =
            f a |> mapSecond (\cmd -> Cmd.batch [ orig_cmd, cmd ])

        syncPintclone eventType name newState =
            case eventType of
                Nothing ->
                    ( { pintclone | state = newState }, Cmd.none )

                Just event ->
                    let (newChat, chatCmd) =
                            Chat.update (Chat.receive <| event name) chat
                    in  ( { pintclone | state = newState , chat = newChat }
                        , handleChatCmd wssend chatCmd
                        )

        mutateRoom eventType name action =
            stateByRoomUpdate state (action name) (wssend <| InfoReq Api.ReqSync)
                |> tupleAndThen (syncPintclone eventType name)
    in
        case msg of
            Api.Joined name ->
                mutateRoom (Just Api.EventJoined) name Room.joins

            Api.Left name ->
                mutateRoom (Just Api.EventLeft) name Room.leaves

            Api.Mastery ->
                mutateRoom Nothing never (always Room.becomeMaster)
                    |> mapSecond (\b -> Cmd.batch [ Ports.selectRoomid (), b ])

            Api.Sync gamestate events ->
                let syncPintclone = { pintclone | chat = Chat.new username events }
                in  case gamestate of
                        Api.Summary scores ->
                            ( newScores scores syncPintclone, Cmd.none )

                        Api.Round drawing round ->
                            ( joinRound  drawing round syncPintclone, Cmd.none )

                        Api.Lobby lobby ->
                            ( fromLobby lobby syncPintclone
                            , Ports.selectRoomid ()
                            )


handleChatCmd : (GameReq -> Cmd Msg) -> Chat.ChatCmd -> Cmd Msg
handleChatCmd wssend chatCmd =
    case chatCmd of
        Chat.DoNothing ->
            Cmd.none
        Chat.Send text ->
            wssend <| Api.ChatReq text
        Chat.UpdateScroll ->
            Ports.bottomScrollChat ()


startRound : Api.RoundStart_ -> Api.Name -> Room -> Canvas -> RoundState_T
startRound { timeout, artist, word } username room canvas  =
    { guess = Guess.new (Just word) timeout
    , room = Room.setArtist artist room
    , canvas = if artist == username then
        Canvas.new [] Canvas.Artist
      else
        Canvas.new [] Canvas.Spectator
    }


updateClassic : Api.Name -> Api.ClassicMsg -> Chat -> RoundState_T -> (Chat, RoundState_T)
updateClassic username msg chat { guess, room, canvas } =
    let
        (newGuess, newRoom, newCanvas) =
            case msg of
                Api.ClaGuessed name ->
                    (guess, room, canvas)
                Api.ClaCorrect completeWord ->
                    ( Guess.update (Guess.RevealAll completeWord) guess
                    , room, canvas
                    )
                Api.ClaTimeout timeout ->
                    ( Guess.update (Guess.SetTimeout timeout) guess
                    , room, canvas
                    )
                Api.RoundOver word _ ->
                    ( Guess.update (Guess.RevealAll word) guess
                    , room, canvas
                    )
                Api.RoundStart { timeout, artist, word } ->
                    ( Guess.new (Just word) timeout
                    , Room.setArtist artist room
                    , if artist == username then
                        Canvas.new [] Canvas.Artist
                      else
                        Canvas.new [] Canvas.Spectator
                    )
                Api.ClaReveal index char ->
                    ( Guess.update (Guess.RevealLetter index char) guess
                    , room, canvas
                    )
    in
        (chat, { canvas = newCanvas, guess = newGuess, room = newRoom })


update : Msg -> Pintclone -> ( Pintclone, Cmd Msg )
update msg ({ roomid, username, openGameRetries, syncRetries } as pintclone_) =
    let
        pintclone =
            { pintclone_ | syncRetries = 0 }

        toCmd maybeCanvasMsg =
            case maybeCanvasMsg of
                Just canvasMsg ->
                    pintclone.wssend <| CanvasReq canvasMsg

                Nothing ->
                    Cmd.none

        report error_msg =
            let
                full_msg =
                    "Failure in '" ++ Api.showRoomID roomid ++ "' for '"
                    ++ Api.showName username ++ "' with the following error: "
                    ++ error_msg
            in
                Api.reportError full_msg |> Cmd.map (\() -> Dummy)

        updateToError title message retry =
            let
                newErrorState =
                    ErrorState { message=message, retry=retry, title=title }
            in
                ( { pintclone | state = newErrorState }, report message )
    in
        case ( msg, pintclone.state ) of
            ( ChatMsg msg_, _ ) ->
                let ( newChat, chatMsg ) = Chat.update msg_ pintclone.chat
                in  ( { pintclone | chat = newChat }
                    , handleChatCmd pintclone.wssend chatMsg
                    )

            ( Info msg_, _ ) ->
                updateInfo msg_ pintclone

            ( Classic (Api.RoundStart msg_), LobbyState { canvas, room } ) ->
                startRound msg_ username room canvas
                    |> \newState ->
                        ( { pintclone | state = RoundState newState }
                        , Cmd.none
                        )

            ( Classic msg_, RoundState state ) ->
                updateClassic username msg_ pintclone.chat state
                    |> \(chat, room) ->
                        ( { pintclone | state = RoundState room, chat = chat }
                        , Cmd.none
                        )

            ( CanvasMsg msg_, state ) ->
                updateCanvas msg_ state
                    |> mapFirst (\x -> { pintclone | state = x })
                    |> mapSecond toCmd

            ( LobbyMsg TogglePassView, LobbyState ({ hideId } as lobby) ) ->
                ( { pintclone
                    | state = LobbyState { lobby | hideId = not hideId }
                  }
                , Cmd.none
                )

            ( LobbyMsg StartGame, LobbyState _ ) ->
                ( pintclone, pintclone.wssend <| InfoReq Api.ReqStart )

            ( ListenError Api.BadSend, _) ->
                if openGameRetries >= 2 then
                    updateToError
                        "Communication error"
                        ("Attempted to connect " ++ toString openGameRetries
                        ++ " times without success. Giving up")
                        (GivenUp (openGameRetries + 1))
                else
                    let
                        attempts =
                            if openGameRetries == 0 then
                                OneAttempt
                            else
                                Attempt (openGameRetries + 1)
                        newErrorState =
                            ErrorState
                                { message = "Websocket connection error"
                                , title = "Communication error (reconnecting)"
                                , retry = attempts
                                }
                    in
                        ( { pintclone | state = newErrorState }
                        , Process.sleep (2.4 * second)
                            |> Task.perform (always Reopen)
                        )

            ( ListenError (Api.DecodeError msg), _) ->
                updateToError "Communication error" msg NoAttempt

            ( Dummy, _ ) ->
                ( pintclone, Cmd.none )

            ( Reopen, _ ) ->
                ( pintclone
                , Api.exitToGame
                    Api.Pintclone roomid username (openGameRetries + 1)
                )

            ( anymsg, anystate ) ->
                if syncRetries >= 3 then
                    updateToError "Synchronisation error"
                        (toString (anymsg, anystate))
                        NoAttempt
                else
                    Debug.log
                        ("Inconsistency:" ++ toString ( anymsg, anystate ))
                        ( { pintclone | syncRetries = syncRetries + 1 }
                        , pintclone.wssend <| InfoReq Api.ReqSync
                        )


subs : Pintclone -> Sub Msg
subs { wslisten, state } =
    let
        listen : Maybe (Api.CanvasMsg -> Msg) -> Result Api.ListenError GameMsg -> Msg
        listen redirectCanvas response =
            case response of
                Err error ->
                    ListenError error

                Ok (Api.InfoMsg msg) ->
                    Info msg

                Ok (Api.ClassicMsg msg) ->
                    Classic msg

                Ok (Api.CanvasMsg msg) ->
                    case redirectCanvas of
                        Just redirect ->
                            redirect msg

                        Nothing ->
                            ListenError <| Api.DecodeError "Recieved a canvas message"

                Ok (Api.ChatMsg msg) ->
                    ChatMsg <| Chat.receive <| Api.EventMessage msg

        cListen canvas =
            listen <| Maybe.map ((<<) CanvasMsg) <| Canvas.subsAdaptor canvas

        toSend =
            case state of
                LobbyState { canvas } ->
                    cListen canvas

                FinalState _ ->
                    listen Nothing

                RoundState { canvas } ->
                    cListen canvas

                Uninit ->
                    listen Nothing

                ErrorState _ ->
                    listen Nothing
    in
        wslisten <| Just toSend


masterDialog : Bool -> Bool -> Api.RoomID -> Html Msg
masterDialog isAlone hideId roomid =
    let
        roomdisplayAttributes =
            [ HA.type_ "text"
            , class "roomid"
            , id (if hideId then "hidden-roomid" else "display-roomid")
            , HA.value <| Api.showRoomID roomid
            , HA.readonly True
            ]
        checkboxAttrs =
            [ HE.onClick <| LobbyMsg TogglePassView
            , id "roomid-toggle"
            , HA.type_ "checkbox"
            ]
        startButton =
            if isAlone then
                H.button [ HA.disabled True ]
            else
                H.button [ HE.onClick <| LobbyMsg StartGame ]
    in
        div [ id "roomiddialog" ]
            [ div []
                [ b [] [ text "Room name:" ]
                , div [ id "roomid-border" ]
                    [ input roomdisplayAttributes []
                    , H.label [ id "roomid-toggle" ]
                        [ input checkboxAttrs [], H.span [] [] ]
                    ]
                ]
            , p [] [ text "share the room name with friends to let them join your game" ]
            , startButton [ text "Start the game!" ]
            ]


view : Pintclone -> List (Html Msg)
view pintclone =
    let
        gameView topBar room canvas =
            div [ id "masterlayout" ]
                [ Room.view room
                , div []
                    [ topBar
                    , H.map CanvasMsg <| Canvas.view canvas
                    ]
                , H.map ChatMsg <| Chat.view pintclone.chat
                ]
    in
        case pintclone.state of
            Uninit ->
                [ h1 [] [ text "Opening game" ] ]

            LobbyState { room, canvas, hideId } ->
                let
                    topBar =
                        if Room.isMaster room then
                            masterDialog (Room.alone room)
                                hideId
                                pintclone.roomid
                          else
                            p [id "top-bar"]
                                [ text "The game leader is waiting to start the game" ]
                in
                    [ gameView topBar room canvas ]

            RoundState { guess, room, canvas } ->
                [ gameView (Guess.view guess) room canvas ]

            FinalState { room, scores } ->
                [ h1 [] [ text "NOT IMPLEMENTED YET" ] ]

            ErrorState { title, message, retry } ->
                let
                    firstParagraph =
                        case retry of
                            GivenUp c ->
                                "After " ++ toString c ++ " tries, we still"
                                ++ " couldn't connect to the game,"
                                ++ " your only option is to try to join a"
                                ++ " different room or change display name."
                            OneAttempt ->
                                "Couldn't connect to the game. Retrying..."
                            Attempt c ->
                                "After " ++ toString c ++ " tries, we still"
                                ++ " couldn't connect to the game. Retrying..."
                            NoAttempt ->
                                "An irreversible error occured and"
                                ++ " your only option is to try to join a"
                                ++ " different room or change display name."
                    secondParagraph =
                        "We are sorry for the inconvenience this causes you."
                    thirdParagraph =
                        "A copy of the following error message has already"
                        ++ " been sent to the developers. We will take care"
                        ++ " that you won't experience this in the future."
                in
                    [ h1 [] [ text title ]
                    , p [] [ text firstParagraph ]
                    , H.a
                        [ href "/friendk/lobby/index.html"]
                        [ H.button [] [ text "Join a different game" ] ]
                    , p [] [ text secondParagraph ]
                    , p [] [ text thirdParagraph ]
                    , pre [] [ text message ]
                    ]
