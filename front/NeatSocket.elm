effect module NeatSocket where { command = MyCmd, subscription = MySub } exposing
  ( send
  , listen
  , keepAlive
  , ServerResponse(Refused,Message)
  )

{-| Copyright (c) 2016, Evan Czaplicki

All rights reserved.


Web sockets make it cheaper to talk to your servers.

Connecting to a server takes some time, so with web sockets, you make that
connection once and then keep using. The major benefits of this are:

  1. It faster to send messages. No need to do a bunch of work for every single
  message.

  2. The server can push messages to you. With normal HTTP you would have to
  keep *asking* for changes, but a web socket, the server can talk to you
  whenever it wants. This means there is less unnecessary network traffic.

The API here attempts to cover the typical usage scenarios, but if you need
many unique connections to the same endpoint, you need a different library.

# Web Sockets
@docs listen, keepAlive, send

-}

import Dict
import Process
import Task exposing (Task)
import WebSocket.LowLevel as WS



-- COMMANDS


type MyCmd msg
  = Send String String


{-| Send a message to a particular address. You might say something like this:

    send "ws://echo.websocket.org" "Hello!"

**Note:** It is important that you are also subscribed to this address with
`listen` or `keepAlive`. If you are not, the web socket will be created to
send one message and then closed. Not good!
-}
send : String -> String -> Cmd msg
send url message =
  command (Send url message)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ (Send url msg) =
  Send url msg



-- SUBSCRIPTIONS

type ServerResponse
    = Refused
    | Message String

type MySub msg
  = Listen String (ServerResponse -> msg)
  | KeepAlive String


{-| Subscribe to any incoming messages on a websocket. You might say something
like this:

    type Msg = Echo String | ...

    subscriptions model =
      listen "ws://echo.websocket.org" Echo

**Note:** If the connection goes down, the effect manager tries to reconnect
with an exponential backoff strategy. Any messages you try to `send` while the
connection is down are queued and will be sent as soon as possible.
-}
listen : String -> (ServerResponse -> msg) -> Sub msg
listen url tagger =
  subscription (Listen url tagger)


{-| Keep a connection alive, but do not report any messages. This is useful
for keeping a connection open for when you only need to `send` messages. So
you might say something like this:

    subscriptions model =
      keepAlive "ws://echo.websocket.org"

**Note:** If the connection goes down, the effect manager tries to reconnect
with an exponential backoff strategy. Any messages you try to `send` while the
connection is down are queued and will be sent as soon as possible.
-}
keepAlive : String -> Sub msg
keepAlive url =
  subscription (KeepAlive url)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
  case sub of
    Listen url tagger ->
      Listen url (tagger >> func)

    KeepAlive url ->
      KeepAlive url



-- MANAGER



type alias State msg =
  { sockets :  SocketsDict
  , queues : QueuesDict
  , subs : SubsDict msg
  , openAttempts : OpenDict
  }

type alias SubsDict msg = Dict.Dict String (List (ServerResponse -> msg))
type alias SocketsDict = Dict.Dict String Connection
type alias QueuesDict = Dict.Dict String (List String)
type alias OpenDict = Dict.Dict String Int


increment : String -> OpenDict -> OpenDict
increment name =
    Dict.update name (\entry -> Just <| Maybe.withDefault 1 entry + 1)

type Connection
  = Opening Int Process.Id
  | Connected WS.WebSocket

init : Task Never (State msg)
init = Task.succeed (State Dict.empty Dict.empty Dict.empty Dict.empty)

-- HANDLE APP MESSAGES

(&>) : Task x a -> Task x b -> Task x b
(&>) t1 t2 = Task.andThen (\_ -> t2) t1

onEffects
  :  Platform.Router msg Msg -> List (MyCmd msg)
  -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmds subs state =
  let
    sendMessagesGetNewQueues : Task x QueuesDict
    sendMessagesGetNewQueues =
      sendMessagesHelp router cmds state.sockets state.queues

    newSubs : SubsDict msg
    newSubs =
      buildSubDict subs Dict.empty

    cleanup : QueuesDict -> Task x (State msg)
    cleanup newQueues =
      let
        newEntries =
          Dict.union newQueues (Dict.map (\k v -> []) newSubs)
        -- Create new connection and add it to the accumulator
        handleNew name _ getNewSockets =
          getNewSockets
            |> Task.andThen (\(openAttempts, newSockets) -> attemptOpen openAttempts router 0 name
            |> Task.map (\(x,pid) -> (x, Dict.insert name (Opening 0 pid) newSockets)))
        -- Add old connection to the accumulator
        handleDups : String -> a -> Connection -> Task x (OpenDict, SocketsDict) -> Task x (OpenDict, SocketsDict)
        handleDups name _ connection =
          Task.map <| Tuple.mapSecond (Dict.insert name connection)
        -- Close connection and do not add it to the accumulator
        handleExistant name connection getNewSockets =
          closeConnection connection &> getNewSockets
        collectNewSockets =
          Dict.merge handleNew handleDups handleExistant newEntries state.sockets
              (Task.succeed (Dict.empty, Dict.empty))
      in
        collectNewSockets
          |> Task.map (\(newAttempts, newSockets) -> State newSockets newQueues newSubs newAttempts)
  in
    sendMessagesGetNewQueues
      |> Task.andThen cleanup


-- Attempts to send the messages in cmds and return QueuesDict updates with
-- messages that couldn't be sent (because the connection wasn't open yet)
sendMessagesHelp : Platform.Router msg Msg -> List (MyCmd msg) -> SocketsDict -> QueuesDict -> Task x QueuesDict
sendMessagesHelp router cmds socketsDict queuesDict =
  case cmds of
    [] ->
      Task.succeed queuesDict

    Send name msg :: rest ->
      case Dict.get name socketsDict of
        Just (Connected socket) ->
          sendHandled name router socket msg
              &> sendMessagesHelp router rest socketsDict queuesDict
        Just (Opening _ _) ->
          sendMessagesHelp router rest socketsDict (Dict.update name (add msg) queuesDict)
        Nothing ->
          sendMessagesHelp router rest socketsDict (Dict.update name (add msg) queuesDict)


buildSubDict : List (MySub msg) -> SubsDict msg -> SubsDict msg
buildSubDict subs dict =
  case subs of
    [] ->
      dict

    (Listen name tagger) :: rest ->
      buildSubDict rest (Dict.update name (add tagger) dict)

    (KeepAlive name) :: rest ->
      buildSubDict rest (Dict.update name (Just << Maybe.withDefault []) dict)


-- head, but for Maybe (List a)
add : a -> Maybe (List a) -> Maybe (List a)
add value maybeList =
  case maybeList of
    Nothing ->
      Just [value]

    Just list ->
      Just (value :: list)



-- HANDLE SELF MESSAGES

type alias CloseReasons =
    { code : Int
    , reason : String
    , wasClean: Bool
    }

type Msg
  = Receive String ServerResponse
  | Die String CloseReasons
  | GoodOpen String WS.WebSocket
  | BadOpen String


sendHandled : String -> Platform.Router msg Msg -> WS.WebSocket -> String -> Task x ()
sendHandled name router socket msg =
    let
        forwardErrors sendFeedback =
            case sendFeedback of
                Nothing -> Task.succeed ()
                Just badSend ->
                    Platform.sendToSelf router <| Receive name Refused
    in
        WS.send socket msg |> Task.andThen forwardErrors



onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
  let
    sends : String -> ServerResponse -> List (Task x ())
    sends name response =
      Dict.get name state.subs
        |> Maybe.withDefault []
        |> List.map (\tagger -> Platform.sendToApp router (tagger response))
  in case selfMsg of
    Receive name response ->
        Task.sequence (sends name response) &> Task.succeed state

    Die name cause ->
      if (Maybe.withDefault 0 (Dict.get name state.openAttempts)) > 5 then
          Task.sequence (sends name Refused) &> Task.succeed state
      else
          let newState newAttempts = { state | openAttempts = increment name newAttempts }
          in  case Dict.get name state.sockets of
                Nothing ->
                  Task.succeed (newState state.openAttempts)

                Just _ ->
                  attemptOpen state.openAttempts router 0 name
                    |> Task.map (\(attempts, pid) ->
                        updateSocket name (Opening 0 pid) (newState attempts)
                    )

    GoodOpen name socket ->
      case Dict.get name state.queues of
        Nothing ->
          Task.succeed (updateSocket name (Connected socket) state)

        Just messages ->
          let
            send msg task =
                sendHandled name router socket msg &> task
            startFold =
              Task.succeed <| removeQueue name <| updateSocket name (Connected socket) state
          in
            List.foldl send startFold messages

    BadOpen name ->
      case Dict.get name state.sockets of
        Nothing ->
          Task.succeed state

        Just (Opening n _) ->
          attemptOpen state.openAttempts router (n + 1) name
            |> Task.map (\(openAttempts, pid) ->
                updateSocket name (Opening (n + 1) pid)
                    { state | openAttempts = openAttempts }
            )

        Just (Connected _) ->
          Task.succeed state

updateSocket : String -> Connection -> State msg -> State msg
updateSocket name connection state =
  { state | sockets = Dict.insert name connection state.sockets }


removeQueue : String -> State msg -> State msg
removeQueue name state =
  { state | queues = Dict.remove name state.queues }



-- OPENING WEBSOCKETS WITH EXPONENTIAL BACKOFF


attemptOpen : OpenDict -> Platform.Router msg Msg -> Int -> String -> Task x (OpenDict, Process.Id)
attemptOpen previousAttempts router backoff name =
  let
    updatedAttempts = increment name previousAttempts
    goodOpen : WS.WebSocket -> Task WS.BadOpen ()
    goodOpen ws =
      Platform.sendToSelf router (GoodOpen name ws)

    badOpen : a -> Task x ()
    badOpen _ =
      Platform.sendToSelf router (BadOpen name)

    actuallyAttemptOpen : Task x ()
    actuallyAttemptOpen =
      open name router
        |> Task.andThen goodOpen
        |> Task.onError badOpen
  in
    Process.spawn (after backoff &> actuallyAttemptOpen)
        |> Task.map (\pid -> (updatedAttempts, pid))


open : String -> Platform.Router msg Msg -> Task WS.BadOpen WS.WebSocket
open name router =
  WS.open name
    { onMessage = \_ msg -> Platform.sendToSelf router (Receive name (Message msg))
    , onClose = \details -> Platform.sendToSelf router (Die name details)
    }


after : Int -> Task x ()
after backoff =
  if backoff < 1 then
    Task.succeed ()
  else
    Process.sleep (toFloat (10 * 2 ^ backoff))



-- CLOSE CONNECTIONS


closeConnection : Connection -> Task x ()
closeConnection connection =
  case connection of
    Opening _ pid ->
      Process.kill pid

    Connected socket ->
      WS.close socket
