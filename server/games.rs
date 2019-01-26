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

type ManagerChannel = mpsc::Sender<ManagerRequest>;

macro_rules! default {
    () => {
        Default::default()
    };
}

quick_error! {
    #[derive(Debug)]
    enum GameInteruption {
        EverybodyLeft {}
        GameError(err: ()) {
            from()
        }
        ManagerChannelError(err: mpsc::SendError<ManagerRequest>) {
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
    /// A player was expected to connect a while ago, tell the game
    /// to drop them if they didn't connect.
    GiveUp(GiveUp),
    /// Player `Id` disconnected.
    Disconnect(Id),
}

/// player `Name` was expected to connect a while ago, tell the game
/// to drop them if they didn't connect. `challenge` identifies the exact
/// connection to give up.
#[derive(Debug)]
struct GiveUp {
    name:      Name,
    challenge: Challenge,
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

/// Specialized structure to manage how the server reacts to people leaving
/// temporarily and announcing their future connection.
///
/// A `HangupChallenger` adapts `ManagerRequest`s relating to client
/// connectivity in a `ManagerChannel`. It sends delayed messages to implement
/// timeouts and such.
struct HangupChallenger {
    newcomings:   FxHashMap<Name, (Id, Challenge)>,
    remaining:    FnvHashMap<Id, Name>,
    challenges:   SlotMap<Challenge, ()>,
    manager_sink: ManagerChannel,
}

impl HangupChallenger {
    /// Create a new `HangupChallenger` with an empty list of
    /// `manager_sink`: where to send `ManagerRequest` back.
    fn new(manager_sink: ManagerChannel) -> Self {
        HangupChallenger {
            newcomings: FxHashMap::with_capacity_and_hasher(4, default!()),
            remaining: FnvHashMap::with_capacity_and_hasher(16, default!()),
            challenges: SlotMap::with_capacity_and_key(4),
            manager_sink,
        }
    }

    /// Accept given `name` temporarily. `name` will be "Given up" in 30
    /// seconds if they do not properly connect (join) by then.
    fn accept(&mut self, name: Name, id: Id) {
        self.drop_in(30, name, id);
    }

    /// Process a `GiveUp` message: send a `Terminate` if the concerned player
    /// didn't yet properly join the game. If the player has already joined
    /// this does nothing.
    fn challenge(
        &mut self,
        GiveUp { name, challenge }: GiveUp,
    ) -> Result<(), GameInteruption> {
        use self::ManagerRequest::Terminate;
        debug!("Challenging giveup {}", name);
        if let Some((id, challenged)) = self.newcomings.remove(&name) {
            if challenge == challenged {
                self.challenges.remove(challenged);
                self.manager_sink.start_send(Terminate(id))?;
                self.manager_sink.poll_complete()?;
            } else {
                self.newcomings.insert(name, (id, challenged));
            }
        }
        Ok(())
    }

    /// Schedule for the termination of given `id`'s connection in `delay`
    /// seconds.
    fn drop_in(&mut self, delay: u64, name: Name, id: Id) {
        let challenge = self.challenges.insert(());
        self.newcomings.insert(name.clone(), (id, challenge));
        let request = ManagerRequest::GiveUp(GiveUp { challenge, name });
        let sink_clone = self.manager_sink.clone();
        warp::spawn(
            sleep(Duration::new(delay, 0))
                .then(move |_| sink_clone.send(request))
                .map(|_| ())
                .map_err(|_| ()),
        );
    }

    /// Confirm connection of `name`. They will not be "Given up" on, unless
    /// they disconnect afterward and do not reconnect in time.
    fn join(&mut self, name: Name) -> Option<Id> {
        self.newcomings.remove(&name).map(|(id, challenge)| {
            info!("Connection with {} established", name);
            self.remaining.insert(id, name);
            self.challenges.remove(challenge);
            id
        })
    }

    /// `id` has disconnected. Provide a small windows of time in which they
    /// can reconnect without issue. If they do not reconnect by then, they
    /// will be "Given up".
    fn disconnect(&mut self, id: Id) -> Option<Name> {
        self.remaining.remove(&id).map(|name| {
            self.drop_in(3, name.clone(), id);
            name
        })
    }

    /// Player `id` was forecefully removed from the game.
    fn remove(&mut self, id: &Id) {
        self.remaining.remove(&id);
    }
}

struct ConnectionManager<G> {
    room_name:    String,
    connections:  FnvHashMap<Id, Connection>,
    manager_sink: ManagerChannel,
    respond:      sync::mpsc::SyncSender<ManagerResponse>,
    hangups:      HangupChallenger,
    game:         G,
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
            if let Some(id) = manager.hangups.join(name) {
                manager.join(id, ws);
            }
        }
        ManagerRequest::Msg(id, msg_val) => {
            let response = manager.game.tells(id, msg_val.into())?;
            manager.broadcast(&response);
        }
        ManagerRequest::Disconnect(id) => {
            let room = &manager.room_name;
            if !manager.connections.remove(&id).is_some() {
                warn!("disconnect non-conn {:?}: {}", id, room);
            }
            if let Some(name) = manager.hangups.disconnect(id) {
                info!("{} in {} temporarily dropped", name, manager.room_name);
            }
        }
        ManagerRequest::Terminate(id) => {
            manager.hangups.remove(&id);
            let removed = manager.connections.remove(&id).is_some();
            match manager.game.leaves(id) {
                LeaveResponse::Successfully(name) => {
                    let room = &manager.room_name;
                    if removed {
                        info!("{} leaves {}", name, room)
                    } else {
                        error!("game user {} wasn't connected: {}", name, room)
                    };
                    let msg = GameMsg::Info(InfoMsg::Left(name));
                    manager.broadcast_to_all(msg);
                }
                LeaveResponse::Empty(name) => {
                    info!("{} leaves {} empty", name, manager.room_name);
                    manager.respond.send(ManagerResponse::Empty);
                    return Err(GameInteruption::EverybodyLeft);
                }
                LeaveResponse::Failed(()) => {
                    let room = &manager.room_name;
                    if removed {
                        warn!("remove an non-track {:?}: {}", id, room)
                    } else {
                        warn!("remove non-con {:?}: {}", id, room)
                    }
                }
            }
        }
        ManagerRequest::GiveUp(give_up) => {
            manager.hangups.challenge(give_up)?;
        }
        ManagerRequest::Expects(name) => match manager.game.joins(name.clone())
        {
            JoinResponse::Accept(id) => {
                info!("Adding {} to {}", name, manager.room_name);
                debug!("with id: {:?}", id);
                let msg = GameMsg::Info(InfoMsg::Joined(name.clone()));
                manager.broadcast_to_all(msg);
                manager.hangups.accept(name, id);
                manager.respond.send(ManagerResponse::Accept);
            }
            JoinResponse::Refuse => {
                warn!("{} refused {}", manager.room_name, name);
                manager.respond.send(ManagerResponse::Refuse);
            }
        },
    };
    Ok(manager)
}

/// Manage what players has access to a specific instance of a game party.
/// One must first be "expected" to then be "accepted" into the game.
pub struct GameRoom {
    manager_sink: ManagerChannel,
    recv_manager: sync::Mutex<sync::mpsc::Receiver<ManagerResponse>>,
}

impl GameRoom {
    /// Create a new empty game room.
    pub fn new(room_name: String) -> Self {
        let (manager_sink, receiv_chan) = mpsc::channel(64);
        let (respond, recv_manager) = sync::mpsc::sync_channel(8);
        let manager = ConnectionManager {
            room_name: room_name.clone(),
            respond,
            connections: FnvHashMap::with_capacity_and_hasher(16, default!()),
            manager_sink: manager_sink.clone(),
            hangups: HangupChallenger::new(manager_sink.clone()),
            game: sketchfighters::Game::new(),
        };
        warp::spawn(
            receiv_chan
                .from_err::<GameInteruption>()
                .fold(manager, oversee)
                .map(|_| panic!("Completed an infinite stream"))
                .map_err(move |close_cond| {
                    info!("Closing {} because: {}", room_name, close_cond);
                }),
        );
        GameRoom {
            manager_sink,
            recv_manager: sync::Mutex::new(recv_manager),
        }
    }

    /// Tells the game that `user` is joining. Returns how it was handled.
    pub fn expect(&mut self, user: Name) -> ManagerResponse {
        let mut chan = self.manager_sink.clone();
        chan.start_send(ManagerRequest::Expects(user.clone()))
            .unwrap();
        chan.poll_complete().unwrap();
        self.recv_manager.lock().unwrap().recv().expect(
            "Error communicating with game manager. The game state is \
             corrupted, there is no point keeping this game room up.",
        )
    }

    /// Tell the game to accept a given connection from `user`.
    /// Returns the server response to the connection
    pub fn accept(&mut self, user: Name, ws: Ws2) -> impl warp::reply::Reply {
        let game_chan = self.manager_sink.clone();
        debug!("accepting {}", user);
        ws.on_upgrade(move |socket| {
            game_chan
                .send(ManagerRequest::Join(user, socket))
                .map_err(|_| ())
                .map(|_| ())
        })
    }
}

impl<G> ConnectionManager<G>
where
    G: Game<Id>,
{
    fn join(&mut self, id: Id, ws: WebSocket) {
        // Split the socket into a sender and receive of messages.
        let (socket_sink, socket_stream) = ws.split();

        // Use an unbounded channel to handle buffering and flushing of
        // messages to the websocket...
        let (buffer_sink, buffer_stream) = mpsc::unbounded();
        warp::spawn(
            buffer_stream
                .map_err(|()| -> warp::Error {
                    panic!("unreachable at games.rs:{}", line!());
                })
                .map(|msg: api::GameMsg| {
                    let sereilized = to_string(&msg).unwrap();
                    debug!("ws send: {}", sereilized);
                    Message::text(sereilized)
                })
                .forward(socket_sink)
                .map(|_| ())
                .map_err(|ws_err| error!("ws send: {}", ws_err)),
        );
        self.connections.insert(id, Connection(buffer_sink));
        let client_stream = socket_stream.map(move |msg| {
            from_slice(msg.as_bytes())
                .map(|v| ManagerRequest::Msg(id, v))
                .unwrap_or_else(|err| {
                    error!("message validation: {}/ `{:?}`", err, msg.to_str());
                    ManagerRequest::Terminate(id)
                })
        });
        let sink_to_manager = self.manager_sink.clone();
        warp::spawn(
            client_stream
                .or_else(move |err| {
                    error!("ws recieve:{}", err);
                    Ok(ManagerRequest::Terminate(id))
                })
                .chain(stream::once(Ok(ManagerRequest::Disconnect(id))))
                .forward(sink_to_manager.sink_map_err(|_| ()))
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
            debug!("sending {:?} to {:?}", msg, id);
            connection.start_send(msg).unwrap_or_else(|e| {
                panic!("game channel failure '{:?}' at game.rs:{}", e, line!())
            });
        }
        for (id, _) in targets {
            debug!("polling {:?}", id);
            let mut connection = &self.connections[id].0;
            connection.poll_complete().unwrap_or_else(|e| {
                panic!("game channel failure '{:?}' at game.rs:{}", e, line!())
            });
        }
    }

    fn broadcast_to_all(&mut self, message: api::GameMsg) {
        debug!("sending {:?} to all", message);
        for connection in self.connections.values_mut() {
            let msg = message.clone();
            connection.0.start_send(msg).unwrap_or_else(|e| {
                panic!("game channel failure '{:?}' at game.rs:{}", e, line!())
            });
        }
        debug!("polling all");
        for connection in self.connections.values_mut() {
            connection.0.poll_complete().unwrap_or_else(|e| {
                panic!("game channel failure '{:?}' at game.rs:{}", e, line!())
            });
        }
    }
}
