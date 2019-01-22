use super::game::{self, JoinResponse, LeaveResponse};
use crate::api::{self, Name};
use log::warn;
use slotmap::{new_key_type, SlotMap};
use std::iter;

type Slab<T> = SlotMap<Id, T>;

new_key_type! {
    pub struct Id;
}

enum Game_ {
    Empty,
    Lobby { room_leader: Id },
    Playing { artist: Id },
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
        use self::api::{GameMsg, GameReq, GameState, InfoMsg, InfoRequest};
        match request {
            GameReq::Info(InfoRequest::Sync_) => {
                let api_state = match self.state {
                    Game_::Empty => GameState::Lobby {
                        players: vec![],
                        master:  false,
                    },
                    Game_::Lobby { room_leader } => {
                        let players = self
                            .players
                            .iter()
                            .map(|(_, p)| p.name.clone())
                            .collect();
                        let master = player == room_leader;
                        GameState::Lobby { players, master }
                    }
                    Game_::Playing { artist } => {
                        let players = self
                            .players
                            .iter()
                            .map(|(_, p)| (p.name.clone(), vec![]))
                            .collect();
                        let artist = self.players[artist].name.clone();
                        let timeout = 30;
                        GameState::Round {
                            players,
                            artist,
                            timeout,
                        }
                    }
                };
                let msg = GameMsg::Info(InfoMsg::Sync_(api_state));
                Ok(broadcast!(to, iter::once((player, msg))))
            }
            GameReq::Info(InfoRequest::Start) => {
                if let Game_::Lobby { room_leader } = self.state {
                    if player != room_leader {
                        warn!(
                            "player {:?} attempted to start game while only \
                             {:?} is allowed to",
                            player, room_leader
                        );
                        Err(())
                    } else {
                        let players: Vec<_> = self
                            .players
                            .iter()
                            .map(|(_, player)| (player.name.clone(), vec![]))
                            .collect();
                        let artist = self.players[room_leader].name.clone();
                        let timeout = 30;
                        let msg =
                            GameMsg::Info(InfoMsg::Sync_(GameState::Round {
                                players,
                                artist,
                                timeout,
                            }));
                        self.state = Game_::Playing {
                            artist: room_leader,
                        };
                        Ok(broadcast!(to_all, self.players, msg.clone()))
                    }
                } else {
                    warn!(
                        "player {:?} tries to start the game while the game \
                         is already running",
                        player
                    );
                    Err(())
                }
            }
            GameReq::Info(InfoRequest::Warn(to_log)) => {
                warn!("{}: {}", &self.players[player].name, to_log);
                Ok(broadcast!(nothing))
            }
            GameReq::Canvas(msg) => {
                let response = self
                    .players
                    .iter()
                    .filter(|(id, _)| *id != player)
                    .map(|(id, _)| (id, GameMsg::Canvas(msg.clone())));
                Ok(broadcast!(to, response))
            }
        }
    }
}
