{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies,
    ViewPatterns, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric,
    DeriveDataTypeable, RankNTypes #-}
module Main where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.List (uncons)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (maybe, catMaybes)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Wai.Routes as Routes
import Wai.Routes (Handler, RouteM, created201, notFound404, conflict409, badRequest400, ok200)
import qualified Network.Wai.Application.Static as SWai
import qualified Network.Wai as Wai
import qualified Text.StringRandom as Rand

import qualified RoomConnection as RC
import qualified RestImpl as API

type RoomID = Text
type RoomMap = Map RoomID RC.Room
type GlobalState = MVar RoomMap
type MutState a = GlobalState -> IO a
data Netpinary = Netpinary GlobalState

{-| In the future, we add redirections to the proper WebSockets urls -}
Routes.mkRoute "Netpinary" [Routes.parseRoutes|
/ws/games/pintclone/+[Text] WebSocketAPI GET
/rooms/create RoomsCreate POST
/rooms/join RoomsJoin POST
/rooms/showAll RoomsShow GET
|]

tshow :: Show a => a -> Text
tshow = T.pack . show

unpackGS :: Netpinary -> GlobalState
unpackGS (Netpinary gs) = gs

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


(<|-|) :: Show a => (Text -> b) -> a -> b
(<|-|) f s = f $ tshow s

(+|+) :: Text -> Text -> Text
(+|+) = T.append



-- *** -- *** -- SERVER STATE TYPE MANIPULATION -- *** -- *** --


createRoom :: MutState RoomID
createRoom gs = do
    roomName <- Rand.stringRandomIO "[a-z0-9]{5}"
    modifyMVar gs $ addRandRoom roomName
    where -- Room name generated outside the lock to not clog it.
        addRandRoom :: RoomID -> RoomMap -> IO (RoomMap, RoomID)
        addRandRoom roomName rm = do
            newRoom <- RC.newRoom
            return ( Map.insert roomName newRoom rm, roomName )



-- *** -- *** -- WEB REQUEST HANDLERS -- *** -- *** --


getRoomsShow :: Handler Netpinary
getRoomsShow = Routes.runHandlerM $ do
    Netpinary globstate <- Routes.sub
    rooms <- liftIO $ readMVar globstate
    Routes.plain <|-| Map.keys rooms


{-| Create a new room, returns redirection to game start lobby with created
    room name when success. Returns an error when failure. -}
postRoomsCreate :: Handler Netpinary
postRoomsCreate = Routes.runHandlerM $ do
    Netpinary gs <- Routes.sub
    roomid <- liftIO $ createRoom gs
    Routes.json roomid
    Routes.status created201


{-| Possible inconsistent states: the post returns the resource for
    the WebSocket, but when client requests the WebSocket, someone
    else connected to it with the same username. -}
postRoomsJoin :: Handler Netpinary
postRoomsJoin = Routes.runHandlerM $ do
    parsedRequest <- fmap API.roomsJoin Routes.jsonBody
    case parsedRequest of
        Left err -> badRequest err
        Right (roomid, username) -> do
            Netpinary gs <- Routes.sub
            roomMap <- liftIO $ readMVar gs
            case Map.lookup roomid roomMap of
                Nothing -> Routes.status notFound404
                Just room -> do
                    isConnected <- liftIO $ RC.isConnected username room
                    if isConnected then Routes.status conflict409
                    else success
    where
        badRequest err = do
            Routes.status badRequest400
            Routes.plain <|-| err

        success = do
            Routes.json API.Pintclone
            Routes.status ok200



getStaticContent :: Wai.Application
getStaticContent =
    SWai.staticApp $
        (SWai.defaultFileServerSettings "build")
            { SWai.ssRedirectToIndex = True
            , SWai.ssListing = Nothing
            }


getWebSocketAPI :: [Text] -> Handler Netpinary
getWebSocketAPI urlparts env req continue = do
    roomMap <- readMVar $ unpackGS $ Routes.envSub env
    maybe
        (continue $ Wai.responseLBS notFound404 [] "Room doesn't exist")
        (\(room, urltail)
                -> RC.app room urltail (Routes.waiReq req) continue)
        $ do
            (h,t) <- uncons urlparts
            room <- Map.lookup h roomMap
            return (room,t)



-- *** -- *** -- APPLICATION HANDLERS -- *** -- *** --


application :: Netpinary -> RouteM ()
application gs = do
    Routes.middleware Routes.logStdoutDev
    Routes.route gs
    Routes.catchall getStaticContent



-- *** -- *** -- MAIN -- *** -- *** --


main :: IO ()
main = do
    putStrLn "Starting server on port 8080"
    globstate <- newMVar Map.empty
    Warp.run 8080 $ Routes.waiApp $ application (Netpinary globstate)
