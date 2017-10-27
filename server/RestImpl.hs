{-# LANGUAGE DeriveGeneric #-}
module RestImpl
    ( roomsCreate
    , roomsJoin
    , Game(Pintclone)
    , Validate(valid)
    , Name
    , RoomID
    , Channel(..)
    , randomRoomID
    , ScoresState
    , LobbyState(..)
    , RoundState(..)
    , GameState(..)
    , InfoMsg(..)
    , InfoRequest(..)
    , CanvasMsg(..)
    ) where

{-| Define all types that come directly from the outside world.
TODO: write tests.
Aeson.encode $ Sync (Round (RoundState [Name "herny"] (Name "Dave") 10))
--> "{"sync":{"round":
--      {"spectators":["herny"],"timeout":10,"artist":"Dave"}
--  }}"

Aeson.encode $
    Sync (Lobby (LobbyState
            [Name "herny", Name "daniel", Name "charles"]
            True))
--> "{"sync":{"lobby":
--      {"master":true,"opponents":["herny","daniel","charles"]}
--  }}"

Aeson.encode $ CnvStart (Point 10 34) (Color "#123456") 34.3
--> "{\"start\":[[10,34],\"#123456\",34.3]}"
-}

import GHC.Generics
import Data.Text (Text, unpack)
import Data.Char (toLower)

import qualified Text.StringRandom as Rand
import qualified Data.Aeson as Aeson
import Data.Aeson
    ( toJSON
    , toEncoding
    , genericToEncoding
    , defaultOptions
    , FromJSON
    , ToJSON
    , Options(..)
    , parseJSON
    )


--- Convenience functions ---


aesonSumOptions :: (String -> String) -> Options
aesonSumOptions tagModifier =
    defaultOptions
        { sumEncoding = Aeson.ObjectWithSingleField
        , constructorTagModifier = tagModifier
        }

(<-|-) :: (Options -> t) -> (String -> String) -> t
(<-|-) pipeTo tagModifier = pipeTo $ aesonSumOptions tagModifier

class Validate a where
    valid :: Text -> Either Text a


--- Raw API types: ---


--- Name ---


newtype Name = Name {unname :: Text} deriving (Generic, Ord, Eq)

instance Show Name where
    show = unpack . unname

instance Validate Name where
    valid = Right . Name

instance FromJSON Name where
    parseJSON = Aeson.withText "Name" $ return . Name

instance ToJSON Name where
    toEncoding (Name { unname }) = toEncoding unname
    toJSON (Name { unname }) = toJSON unname




--- RoomID ---


newtype RoomID = RoomID {unroomid :: Text} deriving (Generic, Ord, Eq)

randomRoomID :: IO RoomID
randomRoomID = fmap RoomID $ Rand.stringRandomIO "[a-z0-9]{5}"

instance Show RoomID where
    show = unpack . unroomid

instance Validate RoomID where
    valid = Right . RoomID

instance FromJSON RoomID where
    parseJSON = Aeson.withText "RoomID" $ return . RoomID

instance ToJSON RoomID where
    toEncoding (RoomID { unroomid }) = toEncoding unroomid
    toJSON (RoomID { unroomid }) = toJSON unroomid



--- Misc ---
-- TODO: implement input validation.


newtype Color = Color {uncolor :: Text} deriving (Generic, Show)

instance FromJSON Color where
    parseJSON = Aeson.withText "Color" $ return . Color

instance ToJSON Color where
    toEncoding (Color { uncolor }) = toEncoding uncolor
    toJSON (Color { uncolor }) = toJSON uncolor



data Point = Point { x :: Int , y :: Int } deriving (Generic, Show)


instance FromJSON Point where
    parseJSON array = do
        [x, y] <- parseJSON array
        return $ Point x y


instance ToJSON Point where
    toEncoding (Point { x, y }) = toEncoding [x, y]
    toJSON (Point { x, y }) = toJSON [x, y]



data Game
    = Pintclone
    deriving (Generic, Show)


instance FromJSON Game
instance ToJSON Game where
    toEncoding Pintclone = toEncoding ("pintclone" :: Text)
    toJSON Pintclone = toJSON ("pintclone" :: Text)


data Channel
    = Info
    | Canvas
    deriving(Show, Eq)

instance Validate Channel where
    valid text
        | text == "info" = Right Info
        | text == "canvas" = Right Canvas
        | otherwise = Left "Invalid channel"


-- URL request types --


data RoomsCreate =
    RoomsCreate
        { game :: Game
        , username :: Name
        }
    deriving (Generic, Show)

instance FromJSON RoomsCreate
instance ToJSON RoomsCreate where
    toEncoding = genericToEncoding defaultOptions


roomsCreate :: Either String RoomsCreate -> Either String (Game, Name)
roomsCreate = fmap (\(RoomsCreate {game, username}) ->
    (game, username))



data RoomsJoin =
    RoomsJoin
        { roomid :: RoomID
        , username :: Name
        }
    deriving (Generic, Show)

instance FromJSON RoomsJoin
instance ToJSON RoomsJoin where
    toEncoding = genericToEncoding defaultOptions


roomsJoin :: Either String RoomsJoin -> Either String (RoomID, Name)
roomsJoin = fmap (\(RoomsJoin {roomid, username}) ->
    (roomid, username))



--- WebSocket Types ---


data ScoresState =
    ScoresState
        { scores :: [(Name, Int)]
        }
    deriving (Generic, Show)


instance ToJSON ScoresState where
    toEncoding = genericToEncoding defaultOptions



data LobbyState =
    LobbyState
        { opponents :: [Name]
        , master :: Bool
        }
    deriving (Generic, Show)


instance ToJSON LobbyState where
    toEncoding = genericToEncoding defaultOptions



data RoundState =
    RoundState
        { spectators :: [Name]
        , artist :: Name
        , timeout :: Int
        }
    deriving (Generic, Show)


instance ToJSON RoundState where
    toEncoding = genericToEncoding defaultOptions



data GameState
    = Summary ScoresState
    | Round RoundState
    | Lobby LobbyState
    deriving (Generic, Show)


instance ToJSON GameState where
    toJSON = Aeson.genericToJSON <-|- map toLower
    toEncoding = Aeson.genericToEncoding <-|- map toLower


data InfoMsg
    = Joined Name
    | Left_ Name
    | Sync GameState
    | Mastery
    deriving (Generic, Show)


instance ToJSON InfoMsg where
    toJSON = Aeson.genericToJSON <-|- (map toLower . filter (/= '_'))
    toEncoding = Aeson.genericToEncoding <-|- (map toLower . filter (/= '_'))


data InfoRequest
    = ReqSync
    | ReqStart
    | ReqWarn Text
    deriving (Generic, Show)


instance FromJSON InfoRequest where
    parseJSON = Aeson.genericParseJSON <-|-
        (\('R':'e':'q':h:tail') -> toLower h : tail')


data CanvasMsg
    = CnvStart Point Color Float
    | CnvContinue Point
    | CnvEnd
    deriving (Generic, Show)


instance FromJSON CanvasMsg where
    parseJSON = Aeson.genericParseJSON <-|-
        (\('C':'n':'v':h:tail') -> toLower h : tail')

instance ToJSON CanvasMsg where
    toEncoding = Aeson.genericToEncoding <-|-
        (\('C':'n':'v':h:tail') -> toLower h : tail')

    toJSON = Aeson.genericToJSON <-|-
        (\('C':'n':'v':h:tail') -> toLower h : tail')
