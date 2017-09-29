{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, NamedFieldPuns,
    MonadComprehensions #-}
module RoomConnection (Room, newRoom, app, isConnected) where

import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, withMVar, readMVar)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forever, forM_)
import Control.Exception (finally)
import Data.Text (Text, append)
import Data.Maybe (isJust, maybe)
import Network.HTTP.Types.Status (notFound404, badRequest400)

data Channel
    = Canvas
    | Info

instance Show Channel where
    show Canvas = "canvas"
    show Info = "info"


type ClientName = Text
data ClientSockets = ClientSockets
    { canvas :: Maybe WS.Connection
    , info :: WS.Connection
    }

type RoomState = Map ClientName ClientSockets
type Room = MVar RoomState
type MutRoom a = Room -> IO a


newRoom :: IO Room
newRoom = newMVar Map.empty


{-| Given an acceptation condition `predicate`
    and a transformation on a RoomState `trans` taking a connection,
    will return a function that takes a connection, accepts and transforms
    it only if `predicate` is true. -}
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


{-| Given `clientName` will accept a new connection if the clientName doesn't
    already exists, otherwise refuses the connection. -}
tryAddClient
    :: ClientName
    -> WS.PendingConnection
    -> MutRoom (Maybe WS.Connection)
tryAddClient clientName =
    tryDo predicate trans
    where
        predicate = Map.notMember clientName
        trans conn = Map.insert clientName (ClientSockets Nothing conn)


{-| Given `clientName` will accept a new canvas connection if the clientName
    doesn't already have a Canvas connection and already exists.
    otherwise refuses the connection-}
tryAddCanvas
    :: ClientName
    -> WS.PendingConnection
    -> MutRoom (Maybe WS.Connection)
tryAddCanvas clientName =
    tryDo predicate trans
    where
        predicate = isJust . (\x -> canvas =<< Map.lookup clientName x)
        trans conn = Map.adjust (\x -> x {canvas = Just conn}) clientName


{-| Broadcast to all clients where filter(client) holds True msg,
    on info channel.  -}
broadcastInfo
    :: WS.WebSocketsData msg
    => (ClientName -> Bool)
    -> msg
    -> MutRoom ()
broadcastInfo filter' msg room =
    let
        sendifok :: Text -> ClientSockets -> IO ()
        sendifok clientName sockets = do
            if filter' clientName then
                WS.sendTextData (info sockets) msg
            else
                return ()
    in
        withMVar room $ Map.foldMapWithKey sendifok


{-| Atomically broadcast of the client leave notification and removing it from
the room -}
disconnect :: ClientName -> MutRoom ()
disconnect clientName =
    flip modifyMVar_ remAndBroadcast
    where
        remAndBroadcast :: RoomState -> IO RoomState
        remAndBroadcast roomState = do
            let disconnected = Map.delete clientName roomState
            forM_-- TODO: use an external API json serialization module isntead of sending the raw name
                (Map.elems disconnected)
                (flip WS.sendTextData ("< " `append` clientName) . info)
            return disconnected


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


-- TODO: properly handle invalid connections
app' :: Channel -> ClientName -> Room -> WS.ServerApp
app' channel clientName room pending = do
    case channel of
        Canvas -> do
            Just conn <- tryAddCanvas clientName pending room
            maintainConnection conn (disconnect clientName) ignoreData room
            -- TODO: only accept and resend data from the current artist
            -- connected.
        Info -> do
            Just conn <- tryAddClient clientName pending room
            broadcastInfo (const True) ("> " `append` clientName) room
            -- TODO: give new client info about who is already connected. Note,
            -- this should be atomic to room variable, to avoid /tromperie/
            maintainConnection conn (disconnect clientName) ignoreData room
    where
        ignoreData :: Text -> x -> IO ()
        ignoreData = const . const $ return () -- ignore 2 arguments, IO ()


{-| Whether username is in given room -}
isConnected :: Text -> MutRoom Bool
isConnected username =
    fmap (Map.member username) . readMVar


app :: Room -> [Text] -> Wai.Application
app room route request respond =
    let
        maybeWaiRespond =
            case route of
                ["canvas", clientName] ->
                    WaiWS.websocketsApp
                        (WS.defaultConnectionOptions)
                        (app' Canvas clientName room)
                        (request)

                ["info", clientName] ->
                    WaiWS.websocketsApp
                        (WS.defaultConnectionOptions)
                        (app' Info clientName room)
                        (request)

                _ ->
                    Just $ Wai.responseLBS notFound404 [] "The websocket doesn't exist"

        badRequestAnswer = -- TODO: understant when this is called
            respond $ Wai.responseLBS badRequest400 [] "Not a WebSocket request"
    in
        maybe badRequestAnswer respond maybeWaiRespond
