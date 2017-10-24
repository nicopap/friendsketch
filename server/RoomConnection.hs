{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, NamedFieldPuns,
    MonadComprehensions #-}
module RoomConnection (Room, newRoom, app, isConnected) where

import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Control.Concurrent (MVar, modifyMVar, newMVar, withMVar, readMVar)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forever, forM_)
import Control.Exception (finally)
import Data.Text (Text)
import Data.Maybe (isJust, fromMaybe)
import Network.HTTP.Types.Status (badRequest400)
import qualified RestImpl as API
import RestImpl (Channel(..))
import qualified Data.Aeson as Aeson

data ClientSockets =
    ClientSockets
        { canvas :: Maybe WS.Connection
        , info :: WS.Connection
        }

instance Show ClientSockets where
    show (ClientSockets { canvas }) =
        "ClientSockets {canvas="
        ++ (if isJust canvas then "(Just <SOCKET>)" else "Nothing")
        ++ ", info=<SOCKET>}"


type RoomState = Map API.Name ClientSockets

type Room = MVar RoomState
type MutRoom a = Room -> IO a


newRoom :: IO Room
newRoom = newMVar Map.empty


{-| Given an acceptation condition `predicate`
and a transformation on a RoomState `trans` taking a connection,
will return a function that takes a connection, accepts and transforms
it only if `predicate` is true.
-}
tryDo
    :: (RoomState -> Bool)
    -> (WS.Connection -> RoomState -> RoomState)
    -> WS.PendingConnection
    -> MutRoom (Maybe WS.Connection)
tryDo predicate trans conn =
    flip modifyMVar atomicTransform
    where
        atomicTransform roomState =
            if predicate roomState then do
                conn' <- WS.acceptRequest conn
                WS.forkPingThread conn' 30
                return ( trans conn' roomState, Just conn' )
            else do
                WS.rejectRequest conn "Unacceptable connection :/"
                return ( roomState, Nothing )


{-| Given `clientName` will accept a new connection
if the clientName doesn't already exists, otherwise
refuses the connection.
-}
tryAddInfo
    :: API.Name
    -> WS.PendingConnection
    -> MutRoom (Maybe WS.Connection)
tryAddInfo clientName =
    tryDo predicate trans
    where
        predicate =
            Map.notMember clientName

        trans conn =
            Map.insert clientName (ClientSockets Nothing conn)


{-| Given `clientName` will accept a new canvas connection
if the clientName doesn't already have a Canvas connection
and already exists.  otherwise refuses the connection
-}
tryAddCanvas
    :: API.Name
    -> WS.PendingConnection
    -> MutRoom (Maybe WS.Connection)
tryAddCanvas clientName =
    tryDo predicate trans
    where
        predicate =
            isJust . (\x -> canvas =<< Map.lookup clientName x)

        trans conn =
            Map.adjust (\x -> x {canvas = Just conn}) clientName



{-| Send to the given client the given message on the info channel.
-}
sendInfo :: API.InfoMsg -> API.Name -> MutRoom ()
sendInfo msg name room = do
    maybeClient <- fmap (Map.lookup name) $ readMVar room
    case maybeClient of
        Just (ClientSockets { info }) ->
            WS.sendTextData info $ Aeson.encode msg
        Nothing ->
            return ()


{-| Broadcast to all clients where filter(client) holds True msg,
on info channel.
previous version:
    let
        sendifok :: API.Name -> ClientSockets -> IO ()
        sendifok clientName sockets = do
            if filter' clientName then
                WS.sendTextData (info sockets) msg
            else
                return ()
    in
        withMVar room $ Map.foldMapWithKey sendifok
-}
broadcastInfo' :: API.InfoMsg -> (API.Name -> Bool) -> MutRoom ()
broadcastInfo' msg predicate =
    let
        sendifok :: API.Name -> ClientSockets -> IO ()
        sendifok name (ClientSockets { info }) =
            if predicate name then
                WS.sendTextData info msg'
            else
                return ()
    in
        flip withMVar $ Map.foldMapWithKey sendifok
    where
        msg' = Aeson.encode msg


{-| broadcast to all clients without exception the given message.
-}
broadcastInfo :: API.InfoMsg -> MutRoom ()
broadcastInfo msg =
    flip withMVar (\cmap ->
        forM_ (Map.elems cmap)
            ( \(ClientSockets { info }) ->
                WS.sendTextData info $ Aeson.encode msg
            )
    )


{-| Notify all players that the given player left the game.
-}
notifyLeft_ :: API.Name -> MutRoom ()
notifyLeft_ name =
    broadcastInfo (API.Left_ name)


{-| Put the game in state of pintclone rounds.
-}
startGame :: MutRoom ()
startGame room = do
    userList <- fmap Map.keys $ readMVar room
    broadcastInfo (syncMsg userList) room
    where
        syncMsg members = API.Sync $ API.Round $ API.RoundState
            (members)
            (head members)
            (300)


{-| TODO: add a proper way to handle game states
-}
sendSync :: API.Name -> MutRoom ()
sendSync target room = do
    msg <- fmap (toGameState . Map.keys) $ readMVar room
    sendInfo (API.Sync msg) target room
    where
        toGameState members =
            API.Lobby (API.LobbyState members True)



{-| Handles and reroutes info request messages gotten from the info socket.
-}
handleInfoReq :: API.Name -> API.InfoRequest -> MutRoom ()
handleInfoReq name msg =
    case msg of
        API.ReqSync ->
            sendSync name

        API.ReqStart ->
            startGame


{-| Maintains a connection to the info websocket of a client.
-}
receiveInfo :: API.Name -> WS.Connection -> MutRoom ()
receiveInfo name socket room = do
    sendSync name room
    finally receiveLoop (notifyLeft_ name room)
    where
        receiveLoop :: IO ()
        receiveLoop = forever $ do
            msg <- fmap Aeson.decode $ WS.receiveData socket
            case msg of
                Just msg' ->
                    handleInfoReq name msg' room
                Nothing ->
                    return ()


maintainConnection
    :: WS.WebSocketsData msg
    => WS.Connection
    -> MutRoom ()
    -> (msg -> MutRoom ())
    -> MutRoom ()
maintainConnection conn onDisconnect onRecieve room =
    finally normalFlow $ onDisconnect room
    where
        normalFlow = forever $ do
            msg <- WS.receiveData conn
            onRecieve msg room


{-| Handle and reroutes connection based on route data turned into application
types.
TODO: properly handle invalid connections
-}
app' :: Channel -> API.Name -> Room -> WS.ServerApp
app' channel clientName room pending = do
    case channel of
        Canvas -> do
            Just conn <- tryAddCanvas clientName pending room
            maintainConnection conn (\_ -> return ()) ignoreData room
            -- TODO: only accept and resend data from the current artist
            -- connected.

        Info -> do
            Just conn <- tryAddInfo clientName pending room
            broadcastInfo (API.Joined clientName) room
            -- TODO: give new client info about who is already connected. Note,
            -- this should be atomic to room variable.
            receiveInfo clientName conn room
    where
        ignoreData :: Text -> x -> IO ()
        ignoreData = const . const $ return () -- ignore 2 arguments, IO ()


{-| Whether username is in given room -}
isConnected :: API.Name -> MutRoom Bool
isConnected username =
    fmap (Map.member username) . readMVar


app :: Room -> Channel -> API.Name -> Wai.Application
app room channel name request continuation =
    let
        maybeWaiRespond =
            WaiWS.websocketsApp
                (WS.defaultConnectionOptions)
                (app' channel name room)
                (request)
    in
        continuation $ fromMaybe
            (Wai.responseLBS badRequest400 [] "Not a WebSocket request")
            (maybeWaiRespond)
