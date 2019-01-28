mod roomids;
#[macro_use]
mod autode;

use percent_encoding::percent_decode;
use quick_error::quick_error;
use serde::{de, Deserialize, Deserializer, Serialize};
use std::{
    fmt,
    str::{FromStr, Utf8Error},
};

quick_error! {
    #[derive(Debug)]
    pub enum NameError {
        InvalidName {}
        TooLong {}
        InvalidFormat(err: Utf8Error) {
            from()
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum ChatMsgError {
        TooLong {}
        InvalidFormat(err: Utf8Error) {
            from()
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum RoomIdError {
        InvalidRoomId {}
        InvalidFormat(err: Utf8Error) {
            from()
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ChatContent(String);

impl fmt::Display for ChatContent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl FromStr for ChatContent {
    type Err = ChatMsgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 300 {
            Ok(ChatContent(String::from(s)))
        } else {
            Err(ChatMsgError::TooLong)
        }
    }
}
impl_deserialize_with_from_str!(ChatContent);

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Hash)]
pub struct Name(String);

impl FromStr for Name {
    type Err = NameError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let decoded = percent_decode(s.as_bytes()).decode_utf8()?;
        if decoded.len() > 30 {
            Err(NameError::TooLong)
        } else {
            Ok(Name(String::from(decoded)))
        }
    }
}
impl_deserialize_with_from_str!(Name);

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct RoomId(roomids::RoomId);

impl RoomId {
    pub fn new_random() -> Self {
        RoomId(roomids::gen())
    }
}
impl FromStr for RoomId {
    type Err = RoomIdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let decoded = percent_decode(s.as_bytes()).decode_utf8()?;
        let validated = roomids::RoomId::try_from(&decoded)
            .ok_or(RoomIdError::InvalidRoomId)?;
        Ok(RoomId(validated))
    }
}
impl_deserialize_with_from_str!(RoomId);

impl fmt::Display for RoomId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text_rep: String = (&self.0).into();
        write!(f, "{}", text_rep)
    }
}

pub type Size = f32;
pub type Score = i32;
pub type Color = String;
pub type Point = (i32, i32);

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum GameEvent {
    Left(Name),
    Joined(Name),
    Message(ChatMsg),
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum RoundSummary {
    Artist(Score),
    Guessed(Score),
    Failed,
    Absent,
}

#[derive(Debug, Serialize, Clone)]
pub struct RoundState {
    pub players: Vec<(Name, Vec<RoundSummary>)>,
    pub artist:  Name,
}

#[derive(Debug, Serialize, Clone)]
pub struct Stroke(pub Point, pub Vec<Point>, pub Color, pub Size);

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum GameState {
    Summary {
        scores: Vec<(Name, Vec<RoundSummary>)>,
    },
    Round(Vec<Stroke>, RoundState),
    Lobby {
        players: Vec<Name>,
        master:  Name,
    },
}

#[derive(Debug, Deserialize)]
pub struct JoinReq {
    pub roomid:   RoomId,
    pub username: Name,
}

#[derive(Debug, Deserialize)]
pub struct CreateReq {
    pub username: Name,
    pub game:     String,
}

#[derive(Debug, Serialize, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CanvasMsg {
    Start(Point, Color, Size),
    End,
    Continue(Point),
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum InfoMsg {
    Joined(Name),
    Left(Name),
    #[serde(rename = "sync")]
    Sync_(GameState, Vec<GameEvent>),
    Mastery,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum InfoRequest {
    #[serde(rename = "sync")]
    Sync_,
    Start,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GameReq {
    Canvas(CanvasMsg),
    Info(InfoRequest),
    Chat(ChatContent),
}

#[derive(Debug, Serialize, Clone)]
pub struct ChatMsg {
    pub content: ChatContent,
    pub author:  Name,
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum GameMsg {
    Canvas(CanvasMsg),
    Info(InfoMsg),
    Chat(ChatMsg),
}
