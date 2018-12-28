use serde::{Deserialize, Serialize};
use std::fmt;
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Serialize)]
pub struct Name(String);

impl Name {
    pub fn try_from(raw: String) -> Result<Self, ()> {
        Ok(Name(raw))
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
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
