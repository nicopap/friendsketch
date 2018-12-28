use futures::{stream, sync::mpsc, Future, Sink, Stream};
use log::{debug, error, info, warn};
use serde_json::{de::from_slice, ser::to_string};
use slotmap::{hop::HopSlotMap, DefaultKey};
use warp::{
    self,
    ws::{Message, WebSocket},
};

use crate::api::{self, Name};

type Slab<T> = HopSlotMap<DefaultKey, T>;
type Id = DefaultKey;

#[derive(Debug)]
enum ClientReq {
    Msg(Id, api::GameReq),
    Terminate(Id),
    Join(Name, WebSocket),
}

enum GameState {
    Empty,
    Lobby { room_leader: Id },
    Playing { artist: Id },
}
struct GameInternal {
    room_name:   String,
    players:     Slab<Player>,
    client_chan: mpsc::Sender<ClientReq>,
    game_state:  GameState,
}

struct Player {
    connection: mpsc::UnboundedSender<api::GameMsg>,
    name:       Name,
}

fn oversee(
    mut game: GameInternal,
    msg: ClientReq,
) -> Result<GameInternal, ()> {
    use self::api::{GameMsg, GameReq, InfoMsg, InfoRequest};
    Ok(match msg {
        ClientReq::Join(name, ws) => {
            info!("Adding {} to {}", &name, &game.room_name);
            game.broadcast(GameMsg::Info(InfoMsg::Joined(name.clone())));
            game.join(name, ws);
            game
        }
        ClientReq::Msg(id, msg_val) => match msg_val {
            GameReq::Info(InfoRequest::Sync_) => {
                use self::GameState::*;
                let api_state = match game.game_state {
                    Empty => api::GameState::Lobby {
                        players: vec![],
                        master:  false,
                    },
                    Lobby { room_leader } => {
                        let players = game
                            .players
                            .iter()
                            .map(|(_, p)| (p.name.clone()))
                            .collect();
                        let master = id == room_leader;
                        api::GameState::Lobby { players, master }
                    }
                    Playing { artist } => {
                        let players = game
                            .players
                            .iter()
                            .map(|(_, p)| (p.name.clone(), vec![]))
                            .collect();
                        let artist = game.players[artist].name.clone();
                        let timeout = 30;
                        api::GameState::Round {
                            players,
                            artist,
                            timeout,
                        }
                    }
                };
                let msg = GameMsg::Info(InfoMsg::Sync_(api_state));
                (&mut game.players[id].connection).send(msg).wait();
                game
            }
            GameReq::Info(InfoRequest::Start) => {
                if let GameState::Lobby { room_leader } = game.game_state {
                    let send_players: Vec<_> = game
                        .players
                        .iter()
                        .map(|(_, player)| (player.name.clone(), vec![]))
                        .collect();
                    let artist = game.players[room_leader].name.clone();
                    game.broadcast(GameMsg::Info(InfoMsg::Sync_(
                        api::GameState::Round {
                            players: send_players,
                            artist,
                            timeout: 60,
                        },
                    )));
                    game.game_state = GameState::Playing {
                        artist: room_leader,
                    };
                    game
                } else {
                    game
                }
            }
            GameReq::Info(InfoRequest::Warn(to_log)) => {
                warn!("{}: {}", &game.players[id].name, to_log);
                game
            }

            GameReq::Canvas(msg) => {
                game.broadcast_except(GameMsg::Canvas(msg), id);
                game
            }
        },
        ClientReq::Terminate(id) => {
            let sender_name = game.players.remove(id).unwrap().name;
            info!("{} left {}", &sender_name, &game.room_name);
            game.broadcast(api::GameMsg::Info(api::InfoMsg::Left(
                sender_name,
            )));
            game
        }
    })
}

#[derive(Clone)]
pub struct GameManager(mpsc::Sender<ClientReq>);
impl GameManager {
    pub fn new(room_name: String) -> Self {
        let (client_chan, receiv_chan) = mpsc::channel(128);
        let game = GameInternal {
            room_name,
            players: Slab::with_capacity(8),
            client_chan: client_chan.clone(),
            game_state: GameState::Empty,
        };
        warp::spawn(receiv_chan.fold(game, oversee).map(|_| ()));
        GameManager(client_chan)
    }

    pub fn join(
        self,
        name: Name,
        ws: WebSocket,
    ) -> impl Future<Item = (), Error = ()> {
        self.0
            .send(ClientReq::Join(name, ws))
            .map_err(|_| ())
            .map(|_| ())
    }
}

impl GameInternal {
    fn join(&mut self, name: Name, ws: WebSocket) {
        // Split the socket into a sender and receive of messages.
        let (ws_send, ws_recv) = ws.split();

        // Use an unbounded channel to handle buffering and flushing of
        // messages to the websocket...
        let (connection, buffed_recv) = mpsc::unbounded();
        warp::spawn(
            buffed_recv
                .map_err(|()| -> warp::Error { unreachable!() })
                .map(|msg: api::GameMsg| {
                    let sereilized = to_string(&msg).unwrap();
                    debug!("ws send: {}", sereilized);
                    Message::text(sereilized)
                })
                .forward(ws_send)
                .map(|_| ())
                .map_err(|ws_err| error!("websocket send error: {}", ws_err)),
        );
        let my_id = self.players.insert(Player { connection, name });
        match self.game_state {
            GameState::Empty => {
                self.game_state = GameState::Lobby { room_leader: my_id };
            }
            _ => {}
        };
        let client_stream =
            ws_recv.then(move |msg| -> Result<ClientReq, ()> {
                let msg = match msg {
                    Ok(msg) => msg,
                    Err(err) => {
                        error!("websocket recieve error: {}", err);
                        return Err(());
                    }
                };
                from_slice(msg.as_bytes())
                    .map(|validated| {
                        debug!("ws recieve: {:?}", validated);
                        ClientReq::Msg(my_id, validated)
                    })
                    .map_err(|err| {
                        error!(
                            "websocket recieve error: {}/ `{:?}`",
                            err,
                            msg.to_str()
                        )
                    })
            });
        warp::spawn(
            client_stream
                .chain(stream::once(Ok(ClientReq::Terminate(my_id))))
                .forward(self.client_chan.clone().sink_map_err(|_| ()))
                .map(|_| ()),
        );
    }

    fn broadcast_except(&mut self, message: api::GameMsg, to_skip: Id) {
        for (id, Player { connection, .. }) in self.players.iter_mut() {
            let msg = message.clone();
            if id != to_skip {
                connection.start_send(msg).expect("everything is fine120");
            }
        }
        for (id, Player { connection, .. }) in self.players.iter_mut() {
            if id != to_skip {
                connection.poll_complete().expect("everything is fine125");
            }
        }
    }

    fn broadcast(&mut self, message: api::GameMsg) {
        for (_, Player { connection, .. }) in self.players.iter_mut() {
            let msg = message.clone();
            connection.start_send(msg).expect("everything is fine133");
        }
        for (_, Player { connection, .. }) in self.players.iter_mut() {
            connection.poll_complete().expect("everything is fine136");
        }
    }
}
