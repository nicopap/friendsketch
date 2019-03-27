pub mod pages;
mod roomids;

use quick_error::quick_error;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use std::{
    default::Default,
    fmt,
    str::{from_utf8, from_utf8_unchecked, FromStr, Utf8Error},
};

quick_error! {
    #[derive(Debug)]
    pub enum RequestError {
        InvalidRoomId {
            display("not a room id")
        }
        Empty {
            display("required")
        }
        EmptyCollection {
            display("The game needs at least one deck to work properly")
        }
        InvalidName {
            display("blank characters at beginning or end")
        }
        TooLong {
            display("too long")
        }
        InvalidFormat(err: Utf8Error) {
            display("broken encoding")
        }
        InvalidSetCount {
            display("impossible to play a negative amount of sets")
        }
        InvalidDistribution {
            display("cannot use a negative/zero distribution")
        }
        InvalidRoundLength {
            display("the round length should be between 10 and 600 seconds")
        }
    }
}

#[derive(Clone, Deserialize, Serialize, Debug)]
pub struct TopicId(pub usize);

#[derive(Clone, Serialize)]
pub struct Topic {
    pub id:          TopicId,
    pub name:        String,
    pub description: String,
}

impl fmt::Debug for Topic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(T#{})", self.name)
    }
}

#[derive(Clone)]
pub struct ChatContent(Vec<u8>);
impl ChatContent {
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}
impl<'de> Deserialize<'de> for ChatContent {
    fn deserialize<D>(deserialize: D) -> Result<ChatContent, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d: Vec<u8> = serde_bytes::deserialize(deserialize)?;
        if d.is_empty() {
            Err(de::Error::custom(RequestError::Empty))
        } else if d.len() < 300 {
            let _ = from_utf8(&d)
                .map_err(RequestError::InvalidFormat)
                .map_err(de::Error::custom)?;
            Ok(ChatContent(d))
        } else {
            Err(de::Error::custom(RequestError::TooLong))
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

impl fmt::Debug for ChatContent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.len() > 30 {
            write!(f, "\"{} bytes message\"", self.0.len())
        } else {
            let text = unsafe { from_utf8_unchecked(&self.0) };
            write!(f, "{}", text)
        }
    }
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct Name(Vec<u8>);

impl<'de> Deserialize<'de> for Name {
    fn deserialize<D>(deserialize: D) -> Result<Name, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d: Vec<u8> = serde_bytes::deserialize(deserialize)?;
        if d.is_empty() {
            Err(de::Error::custom(RequestError::Empty))
        } else if d.len() < 30 {
            let encoded = from_utf8(&d)
                .map_err(RequestError::InvalidFormat)
                .map_err(de::Error::custom)?;
            let maybe_whitespace =
                |x: Option<_>| x.map(char::is_whitespace).unwrap_or(false);
            let starts_space = maybe_whitespace(encoded.chars().next());
            let ends_space = maybe_whitespace(encoded.chars().next_back());
            if starts_space || ends_space {
                Err(de::Error::custom(RequestError::InvalidName))
            } else {
                Ok(Name(d))
            }
        } else {
            Err(de::Error::custom(RequestError::TooLong))
        }
    }
}
impl Serialize for Name {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let unchecked_str = unsafe { from_utf8_unchecked(&self.0) };
        serializer.serialize_str(unchecked_str)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text = unsafe { from_utf8_unchecked(&self.0) };
        f.pad(text)
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(N#{})", self)
    }
}

#[cfg(test)]
pub fn name_from_string(name: String) -> Name {
    Name(name.into_bytes())
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct RoomId(roomids::RoomId);

impl RoomId {
    pub fn new_random() -> Self {
        RoomId(roomids::gen())
    }
}
impl FromStr for RoomId {
    type Err = RequestError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let validated =
            roomids::RoomId::try_from(&s).ok_or(RequestError::InvalidRoomId)?;
        Ok(RoomId(validated))
    }
}
impl<'de> Deserialize<'de> for RoomId {
    fn deserialize<D>(deserialize: D) -> Result<RoomId, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d = <&str>::deserialize(deserialize)?;
        <RoomId>::from_str(d).map_err(de::Error::custom)
    }
}

impl fmt::Display for RoomId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text_rep: String = (&self.0).into();
        f.pad(&text_rep)
    }
}

pub type Size = f32;
pub type Score = u16;
pub type Color = String;
pub type Point = (i32, i32);

#[derive(Debug, Serialize, Clone)]
#[cfg_attr(test, derive(PartialEq))]
#[serde(rename_all = "lowercase")]
pub enum RoundScore {
    Artist(Score),
    Guessed(Score),
    Failed,
    Absent,
}

pub fn score_total(scores: &[RoundScore]) -> Score {
    use self::RoundScore::*;
    scores
        .iter()
        .map(|x| match x {
            Artist(x) | Guessed(x) => *x,
            Failed | Absent => 0,
        })
        .sum()
}

pub type Scoreboard = Vec<(Name, Vec<RoundScore>)>;

#[derive(Debug, Serialize, Clone)]
pub struct Stroke(pub Point, pub Vec<Point>, pub Color, pub Size);

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum GameScreen {
    Scores,
    EndSummary(i16, Vec<Name>),
    Round {
        drawing: Vec<Stroke>,
        artist:  Name,
        timeout: i16,
        word:    Option<Guess>,
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
    Guess(u16),
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
    SyncComplete,
    Voted(Name),
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
    Reveal(u8, char),
    Complete(i16, Scoreboard),
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ScoreScheme {
    Quadratic,
    Linear,
}
impl Default for ScoreScheme {
    fn default() -> Self {
        ScoreScheme::Quadratic
    }
}

#[derive(
    Debug,
    Serialize,
    Deserialize,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
)]
#[serde(rename_all = "lowercase")]
pub enum Difficulty {
    Easy,
    Normal,
    Hard,
}
impl std::str::FromStr for Difficulty {
    type Err = ();

    fn from_str(s: &str) -> Result<Difficulty, ()> {
        match s {
            "easy" => Ok(Difficulty::Easy),
            "normal" => Ok(Difficulty::Normal),
            "hard" => Ok(Difficulty::Hard),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct DeckId {
    pub difficulty: Difficulty,
    pub topic:      TopicId,
}

#[derive(Debug, Deserialize)]
pub struct UserCollection {
    #[serde(deserialize_with = "validate::decks")]
    pub decks: Vec<DeckId>,
    #[serde(deserialize_with = "validate::distrs")]
    pub distrs: (f32, f32, f32),
}

#[derive(Debug, Deserialize)]
pub struct Setting {
    #[serde(deserialize_with = "validate::round_duration")]
    pub round_duration: i16,
    #[serde(deserialize_with = "validate::set_count")]
    pub set_count: u8,
    #[serde(rename = "score_scheme")]
    _score_scheme: ScoreScheme,
    pub collection: UserCollection,
}

#[derive(Debug, Serialize)]
pub struct Deck {
    pub difficulty: Difficulty,
    pub topic:      Topic,
    pub word_count: u16,
}

mod validate {
    use super::{DeckId, RequestError};
    use serde::{de, Deserialize, Deserializer};

    pub(super) fn round_duration<'de, D>(d: D) -> Result<i16, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = Deserialize::deserialize(d)?;
        if value > 600 || value <= 0 {
            Err(de::Error::custom(RequestError::InvalidRoundLength))
        } else {
            Ok(value)
        }
    }

    pub(super) fn set_count<'de, D>(d: D) -> Result<u8, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = Deserialize::deserialize(d)?;
        if value > 10 || value == 0 {
            Err(de::Error::custom(RequestError::InvalidSetCount))
        } else {
            Ok(value)
        }
    }

    pub(super) fn distrs<'de, D>(d: D) -> Result<(f32, f32, f32), D::Error>
    where
        D: Deserializer<'de>,
    {
        let (e, n, h) = Deserialize::deserialize(d)?;
        let total_distr = e + n + h;
        if e < 0.0 || n < 0.0 || h < 0.0 || total_distr <= 0.0 {
            Err(de::Error::custom("invalid distribution"))
        } else {
            Ok((e, n, h))
        }
    }

    pub(super) fn decks<'de, D>(d: D) -> Result<Vec<DeckId>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let decks: Vec<DeckId> = Deserialize::deserialize(d)?;
        if decks.is_empty() {
            Err(de::Error::custom(RequestError::EmptyCollection))
        } else {
            Ok(decks)
        }
    }
}

#[cfg(all(debug_assertions, not(test)))]
#[rustfmt::skip]
mod defaults {
    pub(super) const fn set_count() -> u8 { 1 }
    pub(super) const fn round_duration() -> i16 { 17 }
}

#[cfg(test)]
#[rustfmt::skip]
mod defaults {
    pub(super) const fn set_count() -> u8 { 1 }
    pub(super) const fn round_duration() -> i16 { 1 }
}

#[cfg(not(any(test, debug_assertions)))]
#[rustfmt::skip]
mod defaults {
    pub(super) const fn difficulty_distribution() -> (f32,f32,f32) { (1.0,1.0,1.0) }
    pub(super) const fn set_count() -> u8 { 2 }
    pub(super) const fn round_duration() -> i16 { 80 }
}
