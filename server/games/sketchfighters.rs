use super::game::{self, JoinResponse, LeaveResponse};
use crate::api::{self, Name, Stroke};
use log::{error, warn};
use slotmap::{new_key_type, SlotMap};
use std::iter;

type Slab<T> = SlotMap<Id, T>;

new_key_type! {
    pub struct Id;
}

enum Game_ {
    Empty,
    Lobby { room_leader: Id },
    Playing { drawing: Vec<Stroke>, artist: Id },
}

pub struct Game {
    state:   Game_,
    players: Slab<Player>,
}

struct Player {
    name: Name,
}

macro_rules! broadcast {
    (to_all, $players:expr, $msg:expr) => {
        $players.iter().map(|(id, _)| (id, $msg)).collect()
    };
    (to, $msg:expr) => {
        $msg.collect()
    };
    (nothing) => {
        Vec::new()
    };
}

pub type TellsResp = Result<Vec<(Id, api::GameMsg)>, ()>;

fn sync_msg(msg: api::GameState) -> api::GameMsg {
    use api::{GameMsg::Info, InfoMsg::Sync_};
    Info(Sync_(msg))
}

impl Game {
    fn into_api_state(&self) -> api::GameState {
        use self::api::GameState::{Lobby, Round};
        macro_rules! list {
            ($map:expr) => {
                self.players.iter().map($map).collect();
            };
        }
        match self.state {
            Game_::Empty => {
                error!("Attempt to sync to empty state");
                panic!("sketchfighters:{} unreachable path", line!())
            }
            Game_::Lobby { room_leader } => {
                let players = list!(|(_, p)| p.name.clone());
                let master = self.players[room_leader].name.clone();
                Lobby { players, master }
            }
            Game_::Playing {
                ref drawing,
                artist,
            } => {
                let players = list!(|(_, p)| (p.name.clone(), vec![]));
                let artist = self.players[artist].name.clone();
                let round_state = api::RoundState { players, artist };
                Round(drawing.clone(), round_state)
            }
        }
    }
}
impl game::Game<Id> for Game {
    type Error = ();
    type Request = api::GameReq;
    type Response = api::GameMsg;

    fn new() -> Self {
        Game {
            state:   Game_::Empty,
            players: Slab::with_key(),
        }
    }

    fn joins(&mut self, name: Name) -> JoinResponse<Id> {
        if self.players.values().any(|Player { name: n }| n == &name) {
            JoinResponse::Refuse
        } else {
            let id = self.players.insert(Player { name });
            match self.state {
                Game_::Empty => {
                    self.state = Game_::Lobby { room_leader: id };
                }
                Game_::Lobby { .. } => {}
                Game_::Playing { .. } => {}
            };
            JoinResponse::Accept(id)
        }
    }

    fn leaves(&mut self, player: Id) -> LeaveResponse<()> {
        match self.players.remove(player) {
            Some(Player { name }) => {
                if self.players.is_empty() {
                    LeaveResponse::Empty(name)
                } else {
                    LeaveResponse::Successfully(name)
                }
            }
            None => LeaveResponse::Failed(()),
        }
    }

    fn tells(&mut self, player: Id, request: api::GameReq) -> TellsResp {
        use self::api::{GameMsg, GameReq, InfoRequest};
        Ok(match request {
            GameReq::Info(InfoRequest::Sync_) => None,
            GameReq::Info(InfoRequest::Start) => {
                if let Game_::Lobby { room_leader } = self.state {
                    if player != room_leader {
                        warn!(
                            "player {:?} attempted to start game while only \
                             {:?} is allowed to",
                            player, room_leader
                        );
                        None
                    } else {
                        self.state = Game_::Playing {
                            drawing: vec![],
                            artist:  room_leader,
                        };
                        let msg = sync_msg(self.into_api_state());
                        Some(broadcast!(to_all, self.players, msg.clone()))
                    }
                } else {
                    warn!(
                        "player {:?} tries to start the game while the game \
                         is already running",
                        player
                    );
                    None
                }
            }
            GameReq::Canvas(msg) => {
                if let Game_::Playing {
                    ref mut drawing,
                    artist,
                } = self.state
                {
                    if artist != player {
                        None
                    } else {
                        use self::api::CanvasMsg;
                        match msg {
                            CanvasMsg::Start(point, ref color, size) => {
                                let new_stroke =
                                    Stroke(point, vec![], color.clone(), size);
                                drawing.push(new_stroke);
                            }
                            CanvasMsg::Continue(point) => {
                                drawing.last_mut().unwrap().1.push(point)
                            }
                            CanvasMsg::End => {}
                        }
                        let response = self
                            .players
                            .iter()
                            .filter(|(id, _)| *id != artist)
                            .map(|(id, _)| (id, GameMsg::Canvas(msg.clone())));
                        Some(broadcast!(to, response))
                    }
                } else {
                    None
                }
            }
        }
        .unwrap_or_else(|| {
            let msg = sync_msg(self.into_api_state());
            broadcast!(to, iter::once((player, msg)))
        }))
    }
}
