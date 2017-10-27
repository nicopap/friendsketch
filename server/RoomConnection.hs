module RoomConnection (Room, newRoom, app, isConnected) where

{-| Handles the websockets connected to a Pintclone game.

TODO: use quickcheck to verify concurrent validity of code
TODO: Kick clients that send garbage to server
-}

import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Control.Concurrent (MVar, modifyMVar_, modifyMVar, newMVar, withMVar, readMVar)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forever, forM_)
import Control.Exception (finally)
import Data.ByteString.Lazy (ByteString)
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


{-| The status of the game
-}
data GameState
    = Lobby { master :: API.Name }
    | Round { artist :: API.Name }
    | Empty
    deriving (Show, Eq)



instance Show ClientSockets where
    show (ClientSockets { canvas }) =
        "{c" ++ (if isJust canvas then "+" else "/") ++ ", i+}"


type Clients = Map API.Name ClientSockets

data RoomState =
    RoomState
        { state :: GameState
        , clients :: Clients
        }

instance Show RoomState where
    show (RoomState { state, clients }) = show state ++ ": " ++ show clients

type Room = MVar RoomState
type MutRoom a = Room -> IO a


newRoom :: IO Room
newRoom = newMVar $ RoomState Empty Map.empty


{-| Given an acceptation condition `predicate`
and a transformation on a RoomState `trans` taking a connection,
will return a function that takes a connection, accepts and transforms
it only if `predicate` is true.
-}
tryAccept
    :: (RoomState -> Bool)
    -> (WS.Connection -> RoomState -> RoomState)
    -> WS.PendingConnection
    -> MutRoom (Maybe WS.Connection)
tryAccept predicate trans conn =
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


{-| Atomically checks a a predicate, in case of success, run `overAll`
over all the clients, otherwise runs once `failure`
-}
sendIf
    :: (RoomState -> Bool)
    -> (API.Name -> ClientSockets -> IO ())
    -> IO ()
    -> MutRoom ()
sendIf predicate overAll failure =
    flip withMVar atomicTransform
    where
        atomicTransform :: RoomState -> IO ()
        atomicTransform roomState@(RoomState { clients }) =
            if predicate roomState then
                forM_ (Map.assocs clients) (uncurry overAll)
            else
                failure


{-| tryAccept, but wrapped so it opperates on clients rather than the whole room
-}
tryAcceptclients
    ::(Clients -> Bool)
    -> (WS.Connection -> Clients -> Clients)
    -> WS.PendingConnection
    -> MutRoom (Maybe WS.Connection)
tryAcceptclients predicate trans =
    let
        predicate' (RoomState { clients }) =
            predicate clients

        trans' conn roomstate@(RoomState { clients }) =
            roomstate { clients = trans conn clients }
    in
        tryAccept predicate' trans'


{-| Given `clientName` will accept a new connection
if the clientName doesn't already exists, otherwise
refuses the connection.
-}
tryAddInfo
    :: API.Name
    -> WS.PendingConnection
    -> MutRoom (Maybe WS.Connection)
tryAddInfo clientName =
    tryAcceptclients (Map.notMember clientName) trans
    where
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
    tryAcceptclients (const True) trans
    where
        trans conn =
            Map.adjust (\x -> x {canvas = Just conn}) clientName



{-| Send to the given client the given message on the info channel.
-}
sendInfo :: API.InfoMsg -> API.Name -> MutRoom ()
sendInfo msg name room = do
    maybeClient <- fmap (Map.lookup name . clients) $ readMVar room
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
        flip withMVar $ Map.foldMapWithKey sendifok . clients
    where
        msg' = Aeson.encode msg


{-| broadcast to all clients without exception the given message.
-}
broadcastInfo :: API.InfoMsg -> MutRoom ()
broadcastInfo msg =
    flip withMVar (\(RoomState { clients }) ->
        forM_ (Map.elems clients)
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
    userList <- fmap (Map.keys . clients) $ readMVar room
    broadcastInfo (syncMsg userList) room
-}
startGame :: MutRoom ()
startGame = do
    flip modifyMVar_ atomicTransform
    where
        atomicTransform :: RoomState -> IO RoomState
        atomicTransform (RoomState { clients }) = do
            let members = Map.keys clients
            forM_ (Map.elems clients)
                (\(ClientSockets { info }) ->
                    WS.sendTextData info $ Aeson.encode $ syncMsg members
                )
            return (RoomState { clients = clients, state = Round $ head members })

        syncMsg members = API.Sync $ API.Round $ API.RoundState
            (members)
            (head members)
            (300)




{-| TODO: add a proper way to handle game states
-}
sendSync :: API.Name -> MutRoom ()
sendSync target room = do
    logerr ("sending to " ++ show target) room
    msg <- fmap (toGameState . Map.keys . clients) $ readMVar room
    sendInfo (API.Sync msg) target room
    where
        toGameState members =
            API.Lobby (API.LobbyState (filter (/= target) members) True)



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


{-| Broadcasts content of a CanvasMsg if name is the current artist
TODO: validate data before relaying.
-}
broadcastCanvas :: WS.WebSocketsData a => a -> API.Name -> MutRoom ()
broadcastCanvas msg name room = do
    logerr "brCnv" room
    sendIf predicate overAll failure room
    where
        predicate (RoomState { state }) =
            state == Round { artist = name }

        overAll socketName (ClientSockets { canvas = (Just socket) }) =
            if socketName /= name then
                WS.sendTextData socket msg
            else
                return ()
        overAll _ (ClientSockets { canvas = Nothing }) =
            return ()

        failure =
            sendSync name room


{-| Reroutes data received on this socket to other connected clients.
This only holds if the given socket is identified as the artist's
-}
receiveCanvas :: API.Name -> WS.Connection -> MutRoom ()
receiveCanvas name socket room =
    finally receiveLoop (return ())
    where
        receiveLoop :: IO ()
        receiveLoop = forever $ do
            receivedData <- WS.receiveData socket
            broadcastCanvas (receivedData :: ByteString) name room


{-| Handle and reroutes connection based on route data turned into application
types.
TODO: properly handle invalid connections
-}
app' :: Channel -> API.Name -> Room -> WS.ServerApp
app' channel clientName room pending = do
    case channel of
        Canvas -> do
            attemptConn tryAddCanvas
                (\c -> receiveCanvas  clientName c room)
                ( logerr "Failed to add canvas conn" room )

        Info -> do
            attemptConn tryAddInfo
                (\c -> do
                    broadcastInfo' (API.Joined clientName) (/= clientName) room
                    receiveInfo clientName c room
                )
                ( logerr "name probably already taken" room)
    where
        attemptConn attempt onSuccess onFailure = do
            conn <- attempt clientName pending room
            case conn of
                Just conn' ->
                    onSuccess conn'

                Nothing ->
                    onFailure


logerr :: Show a => String -> MVar a -> IO ()
logerr errmsg room =
    readMVar room
    >>= (\showable -> print $ errmsg ++ ": " ++ show showable)


{-| Whether username is in given room -}
isConnected :: API.Name -> MutRoom Bool
isConnected username =
    fmap (Map.member username . clients) . readMVar


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
