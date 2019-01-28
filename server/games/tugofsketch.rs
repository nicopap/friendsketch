use super::game::{self, JoinResponse, LeaveResponse, TellResponse};
use crate::api::{self, ChatMsg, GameEvent, GameState, Name, Stroke};
use arraydeque::{ArrayDeque, Wrapping};
use log::{error, warn};
use slotmap::{new_key_type, SlotMap};

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
    state:    Game_,
    players:  Slab<Player>,
    game_log: ArrayDeque<[GameEvent; 20], Wrapping>,
}

struct Player {
    name: Name,
}

macro_rules! broadcast {
    (to_all, $msg:expr) => {
        TellResponse::ToAll($msg)
    };
    (to, $players:expr, $msg:expr) => {
        TellResponse::ToList($players.collect(), $msg)
    };
    (to_unique, $player:expr, $msg:expr) => {
        TellResponse::ToList(vec![$player], $msg)
    };
    (nothing) => {
        TellResponse::ToNone
    };
}

fn sync_msg(
    state: GameState,
    events: impl Iterator<Item = GameEvent>,
) -> api::GameMsg {
    use api::{GameMsg::Info, InfoMsg::Sync_};
    Info(Sync_(state, events.collect()))
}

impl Game {
    fn into_sync_msg(&self) -> api::GameMsg {
        use self::GameState::{Lobby, Round};
        macro_rules! list {
            ($map:expr) => {
                self.players.iter().map($map).collect();
            };
        }
        let state = match self.state {
            Game_::Empty => {
                error!("Attempt to sync to empty state");
                panic!("tugofsketch:{} unreachable path", line!())
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
        };
        let events = self.game_log.clone().into_iter();
        sync_msg(state, events)
    }
}
impl game::Game<Id> for Game {
    type Error = ();
    type Request = api::GameReq;
    type Response = api::GameMsg;

    fn new() -> Self {
        Game {
            state:    Game_::Empty,
            players:  Slab::with_key(),
            game_log: ArrayDeque::new(),
        }
    }

    fn joins(&mut self, name: Name) -> JoinResponse<Id> {
        if self.players.values().any(|Player { name: n }| n == &name) {
            JoinResponse::Refuse
        } else {
            self.game_log.push_front(GameEvent::Joined(name.clone()));
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

    fn leaves(&mut self, player: Id) -> LeaveResponse<Id, api::GameMsg, ()> {
        use self::api::{GameMsg::Info, InfoMsg::Mastery};
        match self.players.remove(player) {
            Some(Player { name }) => {
                self.game_log.push_front(GameEvent::Left(name.clone()));
                if let Some((id, _)) = self.players.iter().next() {
                    let response = match self.state {
                        Game_::Lobby {
                            ref mut room_leader,
                        } if *room_leader == player => {
                            *room_leader = id;
                            broadcast!(to_unique, id, Info(Mastery))
                        }
                        Game_::Playing { ref mut artist, .. }
                            if *artist == player =>
                        {
                            *artist = id;
                            broadcast!(nothing)
                        }
                        _ => broadcast!(nothing),
                    };
                    LeaveResponse::Successfully(name, response)
                } else {
                    LeaveResponse::Empty(name)
                }
            }
            None => LeaveResponse::Failed(()),
        }
    }

    fn tells(
        &mut self,
        player: Id,
        request: api::GameReq,
    ) -> Result<TellResponse<Id, api::GameMsg>, ()> {
        use self::api::{GameMsg, GameReq, InfoRequest};
        Ok(match request {
            GameReq::Info(InfoRequest::Sync_) => None,
            GameReq::Info(InfoRequest::Start) => {
                if let Game_::Lobby { room_leader } = self.state {
                    if player != room_leader {
                        warn!("{:?} tries to start, but isn't leader", player);
                        None
                    } else {
                        self.state = Game_::Playing {
                            drawing: vec![],
                            artist:  room_leader,
                        };
                        Some(broadcast!(to_all, self.into_sync_msg()))
                    }
                } else {
                    warn!("{:?} tries to start while game is running", player);
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
                        let msg = GameMsg::Canvas(msg);
                        let others =
                            self.players.keys().filter(|id| *id != artist);
                        Some(broadcast!(to, others, msg))
                    }
                } else {
                    None
                }
            }
            GameReq::Chat(content) => {
                let final_msg = ChatMsg {
                    content,
                    author: self.players[player].name.clone(),
                };
                let log_item = GameEvent::Message(final_msg.clone());
                self.game_log.push_front(log_item);
                Some(broadcast!(to_all, GameMsg::Chat(final_msg)))
            }
        }
        .unwrap_or_else(|| broadcast!(to_unique, player, self.into_sync_msg())))
    }
}
