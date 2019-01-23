mod game;
mod sketchfighters;

use fnv::FnvHashMap;
use futures::{stream, sync::mpsc, Future, Sink, Stream};
use fxhash::FxHashMap;
use log::{debug, error, info, warn};
use quick_error::quick_error;
use serde_json::{de::from_slice, ser::to_string};
use slotmap::{new_key_type, SlotMap};
use std::{sync, time::Duration};
use tokio_timer::sleep;
use warp::{
    self,
    ws::{Message, WebSocket, Ws2},
};

use crate::{
    api::{self, Name},
    games::{
        game::{Game, JoinResponse, LeaveResponse},
        sketchfighters::Id,
    },
};

quick_error! {
    #[derive(Debug)]
    enum GameInteruption {
        EverybodyLeft {}
        GameError(err: ()) {
            from()
        }
    }
}

new_key_type! { struct Challenge; }

/// A message sent to a `ConnectionManager`.
#[derive(Debug)]
enum ManagerRequest {
    /// A message sent through a connected player
    Msg(Id, api::GameReq),
    /// Order the game to drop the given player `Id`
    Terminate(Id),
    /// A connection with given player `Name` has been established
    Join(Name, WebSocket),
    /// A player `Name` is joining, with an imminent connection
    Expects(Name),
    /// player `Name` was expected to connect in too long, tell the game
    /// to drop them. `challenge` identifies the
    GiveUp {
        name:      Name,
        challenge: Challenge,
    },
    /// Player `Id` disconnected.
    Disconnect(Id),
}

/// The result of an attempt to add an user to a game room.
#[derive(Debug)]
pub enum ManagerResponse {
    /// The player was accepted into the room
    Accept,
    /// The player wasn't accepted into the room
    Refuse,
    /// The room should be closed
    Empty,
}

struct ConnectionManager<G> {
    room_name:   String,
    connections: FnvHashMap<Id, Connection>,
    client_chan: mpsc::Sender<ManagerRequest>,
    respond:     sync::mpsc::SyncSender<ManagerResponse>,
    newcomings:  FxHashMap<Name, (Id, Challenge)>,
    remaining:   FnvHashMap<Id, Name>,
    challenges:  SlotMap<Challenge, ()>,
    game:        G,
}

struct Connection(mpsc::UnboundedSender<api::GameMsg>);

fn oversee<G: Game<Id, Error = ()>>(
    mut manager: ConnectionManager<G>,
    msg: ManagerRequest,
) -> Result<ConnectionManager<G>, GameInteruption>
where
    G::Response: Into<api::GameMsg> + Clone,
    G::Request: From<api::GameReq>,
{
    use self::api::{GameMsg, InfoMsg};
    match msg {
        ManagerRequest::Join(name, ws) => {
            if let Some((id, challenge)) = manager.newcomings.remove(&name) {
                info!("Connection with {} established", &name);
                manager.remaining.insert(id, name);
                manager.challenges.remove(challenge);
                manager.join(id, ws);
            } else {
                warn!("Unexpected player {} attempted to connect", &name);
            }
        }
        ManagerRequest::Msg(id, msg_val) => {
            let response = manager.game.tells(id, msg_val.into())?;
            manager.broadcast(&response);
        }
        ManagerRequest::Disconnect(id) => {
            if let Some(name) = manager.remaining.remove(&id) {
                info!("{} in {} temporarly dropped", name, manager.room_name);
                let client_chan = manager.client_chan.clone();
                let challenge = manager.challenges.insert(());
                manager.newcomings.insert(name.clone(), (id, challenge));
                drop_newcoming_in(client_chan, 3, name, challenge);
            }
        }
        ManagerRequest::Terminate(id) => {
            match manager.game.leaves(id) {
                LeaveResponse::Successfully(name) => {
                    if let None = manager.connections.remove(&id) {
                        error!(
                            "{} attempts to leave {}, yet they aren't in it",
                            &name, &manager.room_name
                        )
                    } else {
                        info!("{} leaves {}", &name, &manager.room_name)
                    };
                    manager.remaining.remove(&id);
                    let msg = GameMsg::Info(InfoMsg::Left(name));
                    manager.broadcast_to_all(msg);
                }
                LeaveResponse::Empty(_) => {
                    manager.respond.send(ManagerResponse::Empty);
                    return Err(GameInteruption::EverybodyLeft);
                }
                LeaveResponse::Failed(()) => warn!(
                    "Attempt to terminate innexistant {:?} in {}",
                    id, &manager.room_name
                ),
            };
        }
        ManagerRequest::GiveUp { name, challenge } => {
            use self::ManagerRequest::Terminate;
            if let Some((id, challenged)) = manager.newcomings.remove(&name) {
                if challenge == challenged {
                    manager.challenges.remove(challenged);
                    manager.client_chan.start_send(Terminate(id));
                    manager.client_chan.poll_complete();
                } else {
                    manager.newcomings.insert(name, (id, challenged));
                }
            }
        }
        ManagerRequest::Expects(name) => {
            match manager.game.joins(name.clone()) {
                JoinResponse::Accept(id) => {
                    info!("Adding {} to {}", &name, &manager.room_name);
                    let msg = GameMsg::Info(InfoMsg::Joined(name.clone()));
                    manager.broadcast_to_all(msg);
                    let client_chan = manager.client_chan.clone();
                    let challenge = manager.challenges.insert(());
                    manager.newcomings.insert(name.clone(), (id, challenge));
                    drop_newcoming_in(client_chan, 30, name, challenge);
                    manager.respond.send(ManagerResponse::Accept);
                }
                JoinResponse::Refuse => {
                    warn!("{} refused {}", &manager.room_name, &name);
                    manager.respond.send(ManagerResponse::Refuse);
                }
            }
        }
    };
    Ok(manager)
}

/// Manage what players has access to a specific instance of a game party.
/// One must first be "expected" to then be "accepted" into the game.
pub struct GameRoom {
    send_manager: mpsc::Sender<ManagerRequest>,
    recv_manager: sync::Mutex<sync::mpsc::Receiver<ManagerResponse>>,
}

impl GameRoom {
    /// Create a new empty game room.
    pub fn new(room_name: String) -> Self {
        macro_rules! default {
            () => {
                Default::default()
            };
        }
        let (send_manager, receiv_chan) = mpsc::channel(64);
        let (respond, recv_manager) = sync::mpsc::sync_channel(8);
        let manager = ConnectionManager {
            room_name: room_name.clone(),
            respond,
            connections: FnvHashMap::with_capacity_and_hasher(16, default!()),
            client_chan: send_manager.clone(),
            game: sketchfighters::Game::new(),
            newcomings: FxHashMap::with_capacity_and_hasher(4, default!()),
            remaining: FnvHashMap::with_capacity_and_hasher(16, default!()),
            challenges: SlotMap::with_capacity_and_key(4),
        };
        warp::spawn(
            receiv_chan
                .from_err::<GameInteruption>()
                .fold(manager, oversee)
                .map(|_| unreachable!("Completed an infinite stream"))
                .map_err(move |close_cond| {
                    info!("Closing {} because: {}", room_name, close_cond);
                }),
        );
        GameRoom {
            send_manager,
            recv_manager: sync::Mutex::new(recv_manager),
        }
    }

    /// Tells the game that `user` is joining. Returns how it was handled.
    pub fn expect(&mut self, user: Name) -> ManagerResponse {
        let mut send_chan = self.send_manager.clone();
        send_chan.start_send(ManagerRequest::Expects(user.clone()));
        send_chan.poll_complete();
        self.recv_manager.lock().unwrap().recv().expect(
            "Error communicating with game manager. The game state is \
             corrupted, there is no point keeping this game room up.",
        )
    }

    /// Tell the game to accept a given connection from `user`.
    /// Returns the server response to the connection
    pub fn accept(&mut self, user: Name, ws: Ws2) -> impl warp::reply::Reply {
        let game_chan = self.send_manager.clone();
        ws.on_upgrade(move |socket| {
            game_chan
                .send(ManagerRequest::Join(user, socket))
                .map_err(|_| ())
                .map(|_| ())
        })
    }
}

fn drop_newcoming_in(
    chan: mpsc::Sender<ManagerRequest>,
    seconds: u64,
    name: Name,
    challenge: Challenge,
) {
    let giveup_request = ManagerRequest::GiveUp { challenge, name };
    warp::spawn(
        sleep(Duration::new(seconds, 0))
            .then(move |_| chan.send(giveup_request))
            .map(|_| ())
            .map_err(|_| ()),
    );
}

impl<G> ConnectionManager<G>
where
    G: Game<Id>,
{
    fn join(&mut self, id: Id, ws: WebSocket) {
        macro_rules! validation_error {
            ($err:expr, $msg:expr) => {
                error!("message validation error: {}/ `{:?}`", $err, $msg)
            };
        }
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
        self.connections.insert(id, Connection(connection));
        let client_stream =
            ws_recv.then(move |msg| -> Result<ManagerRequest, ()> {
                let msg = match msg {
                    Ok(msg) => msg,
                    Err(err) => {
                        error!("websocket recieve error: {}", err);
                        return Err(());
                    }
                };
                from_slice(msg.as_bytes())
                    .map(|v| ManagerRequest::Msg(id, v))
                    .map_err(|err| validation_error!(err, msg.to_str()))
            });
        warp::spawn(
            client_stream
                .chain(stream::once(Ok(ManagerRequest::Disconnect(id))))
                .forward(self.client_chan.clone().sink_map_err(|_| ()))
                .map(|_| ()),
        );
    }

    fn broadcast<Msg>(&mut self, targets: &[(Id, Msg)])
    where
        Msg: Into<api::GameMsg> + Clone,
    {
        for (id, message) in targets.iter() {
            let mut connection = &self.connections[id].0;
            let msg = message.clone().into();
            connection.start_send(msg).expect("everything is fine133");
        }
        for (id, _) in targets {
            let mut connection = &self.connections[id].0;
            connection.poll_complete().expect("everything is fine136");
        }
    }

    fn broadcast_to_all(&mut self, message: api::GameMsg) {
        for connection in self.connections.values_mut() {
            let msg = message.clone();
            connection.0.start_send(msg).expect("everything is fine154");
        }
        for connection in self.connections.values_mut() {
            connection.0.poll_complete().expect("everything is fine144");
        }
    }
}
