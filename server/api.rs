mod roomids;
#[macro_use]
mod autode;

use percent_encoding::percent_decode;
use quick_error::quick_error;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use std::{
    fmt,
    str::{from_utf8_unchecked, FromStr, Utf8Error},
};

quick_error! {
    #[derive(Debug)]
    pub enum NameError {
        InvalidName {}
        TooLong {
            display("names should be smaller than 30 bytes")
        }
        InvalidFormat(err: Utf8Error) {
            from()
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum ChatMsgError {
        TooLong {
            display("Chat messages should be smaller than 300 bytes")
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

#[derive(Debug, Clone)]
pub struct ChatContent(Vec<u8>);
impl ChatContent {
    pub fn as_bytes(&self) -> &[u8] {
        return &self.0;
    }
}
impl<'de> Deserialize<'de> for ChatContent {
    fn deserialize<D>(deserialize: D) -> Result<ChatContent, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d: Vec<u8> = serde_bytes::deserialize(deserialize)?;
        if d.len() < 300 {
            Ok(ChatContent(d))
        } else {
            Err(de::Error::custom(ChatMsgError::TooLong))
        }
    }
}
impl Serialize for ChatContent {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let unchecked_str = unsafe { from_utf8_unchecked(&self.0) };
        serializer.serialize_str(unchecked_str)
    }
}

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
pub type Score = u16;
pub type Color = String;
pub type Point = (i32, i32);

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum RoundScore {
    Artist(Score),
    Guessed(Score),
    Failed,
    Absent,
}

pub type Scoreboard = Vec<(Name, Vec<RoundScore>)>;

#[derive(Debug, Serialize, Clone)]
pub struct Stroke(pub Point, pub Vec<Point>, pub Color, pub Size);

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum GameScreen {
    Scores,
    EndSummary,
    Round {
        drawing: Vec<Stroke>,
        artist:  Name,
        timeout: i16,
    },
    Lobby {
        master: Name,
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

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GameReq {
    Canvas(CanvasMsg),
    Chat(ChatContent),
    Sync,
    Start,
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
    #[serde(rename = "event")]
    VisibleEvent(VisibleEvent),
    #[serde(rename = "event")]
    HiddenEvent(HiddenEvent),
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum Guess {
    Artist(String),
    Guess(usize),
}

#[derive(Debug, Serialize, Clone)]
pub struct RoundStart {
    pub timeout: i16,
    pub artist:  Name,
    pub word:    Guess,
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum VisibleEvent {
    Guessed(Name),
    Left(Name),
    Joined(Name),
    Message(ChatMsg),
    SyncStart(Name),
    SyncOver(String),
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum HiddenEvent {
    Correct(String),
    TimeoutSync(i16),
    Sync {
        scores:  Scoreboard,
        screen:  GameScreen,
        history: Vec<VisibleEvent>,
    },
    Mastery,
    Over(String, Vec<(Name, RoundScore)>),
    Start(RoundStart),
    Reveal(usize, char),
}
