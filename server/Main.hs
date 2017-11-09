{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies,
    ViewPatterns, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric,
    DeriveDataTypeable, RankNTypes #-}
module Main where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (maybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Wai.Routes as Routes
import Wai.Routes (Handler, created201, notFound404, badRequest400, ok200)
import qualified Network.Wai.Application.Static as SWai
import qualified Network.Wai as Wai

import qualified Pintclone
import qualified API
import API (RoomID)
import qualified WebSocketGame as WSGame


data ServerState wsgame
    = ServerState { rooms :: Map RoomID (MVar wsgame) }


newtype Netpinary
    = Netpinary {unnetpinary :: MVar (ServerState Pintclone.Room) }

{-| In the future, we add redirections to the proper WebSockets urls -}
Routes.mkRoute "Netpinary" [Routes.parseRoutes|
/ws/games/pintclone/+[Text] WebSocketAPI GET
/rooms/create RoomsCreate POST
/rooms/join RoomsJoin POST
/rooms/showAll RoomsShow GET
|]

tshow :: Show a => a -> Text
tshow = T.pack . show

(|>>) :: a -> (a -> b) -> b
(|>>) = flip ($)
infixl 1 |>>

(+|+) :: Text -> Text -> Text
(+|+) = T.append

getServerState :: Routes.HandlerM Netpinary m (Map RoomID (MVar Pintclone.Room))
getServerState = Routes.sub
    >>= liftIO . readMVar . unnetpinary
    |>> fmap rooms
    >>= (return $!)


parseRequest
    :: Either String val
    -> (val -> Routes.HandlerM s m ())
    -> Routes.HandlerM s m ()
parseRequest (Left err) _ = do
    Routes.status badRequest400
    Routes.plain $ tshow err
parseRequest (Right val) continue = continue val



-- *** -- *** -- SERVER STATE TYPE MANIPULATION -- *** -- *** --


createRoom :: API.Name -> MVar (ServerState Pintclone.Room) -> IO RoomID
createRoom name serverRef = do
    roomName <- API.randomRoomID
    modifyMVar serverRef $ addRandRoom roomName
    where
        addRandRoom
            :: RoomID -> ServerState Pintclone.Room
            -> IO (ServerState Pintclone.Room, RoomID)
        addRandRoom roomName serverState@ServerState{rooms} = do
            newRoom <- WSGame.new name
            return $!
                ( serverState { rooms = Map.insert roomName newRoom rooms }
                , roomName
                )



-- *** -- *** -- WEB REQUEST HANDLERS -- *** -- *** --


getRoomsShow :: Handler Netpinary
getRoomsShow = Routes.runHandlerM $ do
    rooms <- getServerState
    Routes.plain $ tshow $ Map.keys rooms


{-| Create a new room, returns redirection to game start lobby with created
    room name when success. Returns an error when failure. -}
postRoomsCreate :: Handler Netpinary
postRoomsCreate = Routes.runHandlerM $ do
    request <- API.roomsCreate <$> Routes.jsonBody
    parseRequest request $ \(_, username) -> do
        serverState <- unnetpinary <$> Routes.sub
        roomid <- liftIO $ createRoom username serverState
        Routes.json roomid
        Routes.status created201


{-| Possible inconsistent states: the post returns the resource for
    the WebSocket, but when client requests the WebSocket, someone
    else connected to it with the same username. -}
postRoomsJoin :: Handler Netpinary
postRoomsJoin = Routes.runHandlerM $ do
    request <- API.roomsJoin <$> Routes.jsonBody
    parseRequest request $ \(roomid, _) -> do
        roomMap <- getServerState
        case Map.member roomid roomMap of
            True ->
                Routes.json API.Pintclone >> Routes.status ok200

            False ->
                Routes.status notFound404


getStaticContent :: Wai.Application
getStaticContent =
    SWai.staticApp $
        (SWai.defaultFileServerSettings "build") -- TODO: remove ()?
            { SWai.ssRedirectToIndex = True
            , SWai.ssListing = Nothing
            }


{-| with urlparts == [RoomID, RC.Channel, Name], reroutes the request to the
correct WebSocket handler, if RoomID exists.
-}
getWebSocketAPI :: [Text] -> Handler Netpinary
getWebSocketAPI urlparts env req continue = do
    serverState <- readMVar $ unnetpinary $ Routes.envSub env
    case maybeApp urlparts serverState of
        Right app ->
            app (Routes.waiReq req) continue

        Left err ->
            continue $ Wai.responseBuilder notFound404 [] $
                encodeUtf8Builder err
    where
        -- maybeApp return a Left Text (with pertinent error message)
        -- if any of its component fails at parsing.
        maybeApp
            :: WSGame.WebSocketGame x
            => [Text] -> ServerState x -> Either Text Wai.Application
        maybeApp [rawroomid, rawchannel, rawname] serverState =
            WSGame.app
                <$> API.valid rawchannel
                <*> API.valid rawname
                <*> (findRoom serverState =<< API.valid rawroomid)
        maybeApp _ _ = Left "Invalid request path"

        findRoom :: ServerState x -> API.RoomID -> Either Text (MVar x)
        findRoom ServerState{rooms} roomid =
            maybe (Left "The given roomid doesn't exist") (Right)
                (Map.lookup roomid rooms)



-- *** -- *** -- MAIN -- *** -- *** --


main :: IO ()
main = do
    putStrLn "Starting server"
    initialState <- newMVar $ ServerState{rooms = Map.empty}
    Warp.run 8080 $ Routes.waiApp $ do
        Routes.middleware Routes.logStdoutDev
        Routes.route (Netpinary initialState)
        Routes.catchall getStaticContent
