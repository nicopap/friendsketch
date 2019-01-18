mod roomids;

use percent_encoding::percent_decode;
use quick_error::quick_error;
use serde::{Deserialize, Serialize};
use std::{
    fmt,
    str::{FromStr, Utf8Error},
};

quick_error! {
    #[derive(Debug)]
    pub enum NameError {
        InvalidName {}
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

#[derive(
    Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Serialize, Deserialize,
)]
pub struct Name(String);

impl Name {
    pub fn try_from(raw: String) -> Result<Self, NameError> {
        Ok(Name(raw))
    }
}
impl FromStr for Name {
    type Err = NameError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let decoded = percent_decode(s.as_bytes()).decode_utf8()?;
        Name::try_from(decoded.to_string())
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Deserialize, Hash)]
pub struct RoomId(roomids::RoomId);

impl RoomId {
    pub fn try_from(raw: &str) -> Result<Self, RoomIdError> {
        let validated = roomids::RoomId::try_from(raw)
            .ok_or(RoomIdError::InvalidRoomId)?;
        Ok(RoomId(validated))
    }

    pub fn new_random() -> Self {
        RoomId(roomids::gen())
    }
}
impl FromStr for RoomId {
    type Err = RoomIdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let decoded = percent_decode(s.as_bytes()).decode_utf8()?;
        RoomId::try_from(&decoded)
    }
}

impl fmt::Display for RoomId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text_rep: String = (&self.0).into();
        write!(f, "{}", text_rep)
    }
}

#[derive(Debug, Serialize, Clone, Deserialize)]
pub struct Size(pub f32);

pub type LogString = String;
pub type Score = i32;
pub type Color = String;
pub type Point = (i32, i32);

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum RoundSummary {
    Artist(Score),
    Guessed(Score),
    Failed,
    Absent,
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum GameState {
    Summary {
        scores:  Vec<(Name, Vec<RoundSummary>)>,
        timeout: i32,
    },
    Round {
        players: Vec<(Name, Vec<RoundSummary>)>,
        artist:  Name,
        timeout: i32,
    },
    Lobby {
        players: Vec<Name>,
        master:  bool,
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
    Sync_(GameState),
    Mastery,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum InfoRequest {
    #[serde(rename = "sync")]
    Sync_,
    Start,
    Warn(LogString),
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GameReq {
    Canvas(CanvasMsg),
    Info(InfoRequest),
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum GameMsg {
    Canvas(CanvasMsg),
    Info(InfoMsg),
}
