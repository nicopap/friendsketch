{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields,
    NamedFieldPuns #-}
module RestImpl
    ( roomsCreate
    , roomsJoin
    , joinIndex
    , Game
    , Name
    , RoomID
    , JoinIndex
    , APIValidation
    ) where

import GHC.Generics
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import  Data.Aeson (decode, toJSON, toEncoding, genericToEncoding, defaultOptions, FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)


-- TYPES --

type APIValidation = (ByteString -> Maybe Aeson.Value)
-- TODO: implement input validation.
newtype Name = Name {unname :: Text} deriving (Generic, Show)
newtype RoomID = RoomID {unroomid :: Text} deriving (Generic, Show)


data Game
    = Pintclone
    deriving (Generic, Show)

data RoomsCreate = RoomsCreate
    { game :: Game
    , username :: Name
    } deriving (Generic, Show)


data RoomsJoin = RoomsJoin
    { roomid :: RoomID
    , username :: Name
    } deriving (Generic, Show)


data JoinIndex = JoinIndex
    { username :: Name
    } deriving (Generic, Show)

instance ToJSON JoinIndex where toEncoding = genericToEncoding defaultOptions
instance ToJSON Name where toEncoding = genericToEncoding defaultOptions
instance ToJSON RoomID where toEncoding = genericToEncoding defaultOptions
instance ToJSON Game where toEncoding = genericToEncoding defaultOptions
instance ToJSON RoomsCreate where toEncoding = genericToEncoding defaultOptions
instance ToJSON RoomsJoin where toEncoding = genericToEncoding defaultOptions


instance FromJSON JoinIndex
instance FromJSON Name
instance FromJSON RoomID
instance FromJSON Game
instance FromJSON RoomsCreate
instance FromJSON RoomsJoin


joinIndex :: ByteString -> Maybe Aeson.Value
joinIndex bs = toJSON <$> (decode bs :: Maybe JoinIndex)

roomsCreate :: Either String RoomsCreate -> Either String (Text, Text)
roomsCreate = fmap (\(RoomsCreate {game, username}) ->
    (T.pack $ show game, unname username))


roomsJoin :: Either String RoomsJoin -> Either String (Text, Text)
roomsJoin = fmap (\(RoomsJoin {roomid, username}) ->
    (unroomid roomid, unname username))
