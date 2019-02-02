module Pintclone.Game exposing
    ( Game
    , Msg
    , Event
    , GameCmd(..)
    , sync
    , view
    , update
    , receive
    , subs
    )


import Tuple exposing (first, mapFirst, mapSecond)
import Time exposing (every, second)

import Html as H exposing (Html, div, p, b, h1, h3, text, pre, input)
import Html.Attributes as HA exposing (id, class, href)
import Html.Events as HE

import Api
import Ports
import Pintclone.Room as Room exposing (Room)
import Pintclone.Guess as Guess exposing (Guess)
import Canvas exposing (Canvas)
import Chat exposing (Chat)


type GamePart
    = LobbyState { hideId : Bool }
    | Round Guess
    | BetweenRound

type alias Game_ =
    { room : Room
    , canvas : Canvas
    , gamePart : GamePart
    , chat : Chat
    }


type Game = Game Game_


sync : Api.GameState -> Api.Name -> ( Game, GameCmd )
sync { screen, history, scores } username =
    let (gamePart, makeRoom, canvasInit, cmd) = case screen of
        Api.Summary ->
            Debug.crash "final game summary not supported yet"

        Api.RoundScores ->
            ( BetweenRound
            , Room.joinBreak
            , Canvas.Demo
            , Cmd.none
            )
        Api.Round { drawing, artist, timeout } ->
            ( Round <| Guess.new Nothing timeout
            , Room.joinRound artist
            , Canvas.Receiver drawing
            , Cmd.none
            )
        Api.Lobby master ->
            ( LobbyState { hideId = True }
            , Room.newInLobby master
            , Canvas.Demo
            , Ports.selectRoomid ()
            )
    in
        ( Game
            { room = makeRoom username scores
            , canvas = Canvas.new canvasInit
            , gamePart = gamePart
            , chat = Chat.new username history
            }
        , Execute cmd
        )


type alias Event = Result Api.HiddenEvent Api.VisibleEvent

type Msg
    = Event Event
    | CanvasMsg Canvas.Msg
    | ChatMsg Chat.Msg
    | StartGame
    | TogglePassView
    | TickDown

receive : Api.GameMsg -> Msg
receive msg =
    case msg of
        Api.CanvasMsg canvasMsg -> CanvasMsg <| Canvas.Server canvasMsg
        Api.VisibleEvent event -> Event <| Ok event
        Api.HiddenEvent event -> Event <| Err event

type GameCmd
    = Send Api.GameReq
    | Execute (Cmd Msg)
doNothing : GameCmd
doNothing = Execute (Cmd.none)


tickTimer : Game_ -> Game
tickTimer ({ gamePart } as game) =
    case gamePart of
        Round guess ->
            Game { game | gamePart = Round (Guess.update Guess.TickDown guess) }
        _ -> Game game


togglePass : Game_ -> Game
togglePass ({ gamePart } as game) =
    case gamePart of
        LobbyState { hideId } ->
            Game { game | gamePart = LobbyState { hideId = not hideId } }
        _ -> Game game


update : Msg -> Game -> ( Game, GameCmd )
update msg (Game ({ canvas, chat } as game)) =
    case msg of
        Event event ->
            updateByEvent event game

        CanvasMsg canvasMsg ->
            let (newCanvas, canvasCmd) = Canvas.update canvasMsg canvas
                cmd = canvasCmd
                        |> Maybe.map (Send << Api.CanvasReq)
                        |> Maybe.withDefault doNothing
            in
                ( Game { game | canvas = newCanvas } , cmd )

        ChatMsg msg_ ->
            chatMsg msg_ chat |> mapFirst (\c -> Game { game | chat = c })

        TogglePassView -> (togglePass game, doNothing)
        StartGame -> (Game game, Send Api.ReqStart)
        TickDown -> (tickTimer game, doNothing)


chatMsg : Chat.Msg -> Chat -> ( Chat, GameCmd )
chatMsg msg chat =
    let
        handleChatCmd chatCmd =
            case chatCmd of
                Chat.DoNothing -> doNothing
                Chat.Send text -> Send <| Api.ChatReq text
                Chat.UpdateScroll -> Execute <| Ports.bottomScrollChat ()
    in
        Chat.update msg chat |> mapSecond handleChatCmd

chatEvent : Api.VisibleEvent -> Chat -> ( Chat, GameCmd )
chatEvent = chatMsg << Chat.receive

updateByEvent : Event -> Game_ -> ( Game, GameCmd )
updateByEvent event ({ room, canvas, gamePart, chat} as game) =
    let simpleChatMsg message =
            chatEvent message chat |> mapFirst (\c -> Game { game | chat = c })

        guessMsg message guess =
            let guess_ = Guess.update message guess
            in  (Game { game | gamePart = Round guess_ }, doNothing)

    in case (event, gamePart) of
        (Err (Api.EhSync gameState), _) ->
            sync gameState (Room.myName room)

        (Err Api.EhMastery, LobbyState _) ->
            (Game { game | room = Room.becomeMaster room } , doNothing)

        (Err (Api.EhCorrect w  ), Round g) -> guessMsg (Guess.RevealAll w) g
        (Err (Api.EhReveal  i c), Round g) -> guessMsg (Guess.RevealOne i c) g
        (Err (Api.EhTimeout t  ), Round g) -> guessMsg (Guess.SetTimeout t) g

        (Ok ((Api.EvGuessed _) as message), _) -> simpleChatMsg message
        (Ok ((Api.EvMessage _) as message), _) -> simpleChatMsg message
        (Ok ((Api.EvStart   _) as message), _) -> simpleChatMsg message
        (Ok ((Api.EvOver    _) as message), _) -> simpleChatMsg message

        (Ok ((Api.EvLeft player) as chatEvent_), _) ->
            let (newChat, chatCmd) = chatEvent chatEvent_ chat
                newRoom = Room.leaves player room
            in
                if Room.alone newRoom then
                    (Game
                        { chat = newChat
                        , room = Room.newInLobby
                            (Room.myName room) (Room.myName room) []
                        , gamePart = LobbyState { hideId = True }
                        , canvas = Canvas.new Canvas.Demo
                        }
                    , chatCmd
                    )

                else
                    (Game { game | chat = newChat , room = newRoom }, chatCmd)

        (Ok ((Api.EvJoined player) as chatEvent_), _) ->
            let (newChat, chatCmd) = chatEvent chatEvent_ chat
                newRoom = Room.joins player room
            in
                (Game { game | chat = newChat , room = newRoom } , chatCmd)


        (Err (Api.EhOver word scores), Round _ ) ->
            let (newChat, chatCmd) = chatEvent (Api.EvOver word) chat
                newRoom = Room.syncScores scores room
                newPart = BetweenRound
                newGame = Game
                    { game
                        | chat = newChat
                        , gamePart = newPart
                        , room = newRoom
                    }
            in
                (newGame, chatCmd)

        (Err (Api.EhStart { timeout, artist, word }), _) ->
            let (newChat, chatCmd) = chatEvent (Api.EvStart artist) chat
                newGamePart = Round (Guess.new (Just word) timeout)
                newRoom = Room.setArtist artist room
                newCanvas =
                    if Room.amArtist newRoom then
                        Canvas.new Canvas.Sender
                    else
                        Canvas.new <| Canvas.Receiver []
                newGame = Game
                    { room = newRoom
                    , canvas = newCanvas
                    , gamePart = newGamePart
                    , chat = newChat
                    }
            in
                (newGame, chatCmd)

        (anyElse, _) ->
            Debug.log "inconsistency" anyElse |> (\_ -> (Game game, doNothing))


subs : Game -> Sub Msg
subs (Game { gamePart }) =
    case gamePart of
        Round _ -> every second (always TickDown)
        _ -> Sub.none


masterDialog : Bool -> Bool -> Api.RoomID -> Html Msg
masterDialog canStart hideId roomid =
    let
        roomdisplayAttributes =
            [ HA.type_ "text"
            , class "roomid"
            , id (if hideId then "hidden-roomid" else "display-roomid")
            , HA.value <| Api.showRoomID roomid
            , HA.readonly True
            ]
        checkboxAttrs =
            [ HE.onClick TogglePassView
            , id "roomid-toggle"
            , HA.type_ "checkbox"
            ]
        startButton =
            if canStart then
                H.button [ HE.onClick StartGame ]
            else
                H.button [ HA.disabled True ]
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


view : Api.RoomID -> Game -> Html Msg
view roomid (Game { room, gamePart, chat, canvas }) =
    let
        gameView topBar =
            div [ id "masterlayout" ]
                [ Room.view room
                , div [] [ topBar, H.map CanvasMsg <| Canvas.view canvas ]
                , H.map ChatMsg <| Chat.view chat
                ]
    in
        case gamePart of
            LobbyState { hideId }  ->
                let
                    flavorText = "The game leader is waiting to start the game"
                    topBar =
                        if Room.isMaster room then
                            masterDialog (Room.canStart room) hideId roomid
                          else
                            p [id "top-bar"] [ text flavorText]
                in
                    gameView topBar

            Round guess -> gameView (Guess.view guess)
            BetweenRound -> h1 [] [ text "NOT IMPLEMENTED YET" ]

