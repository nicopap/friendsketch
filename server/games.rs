mod game;
mod tugofsketch;

use futures::{stream, sync::mpsc, Future, Sink, Stream};
use log::{debug, error, info, warn};
use quick_error::quick_error;
use serde::de::Error;
use serde_json::{de::from_str, ser::to_string};
use slotmap::SecondaryMap;
use std::{sync, time::Duration};
use tokio_timer::sleep;
use warp::{
    self,
    ws::{Message, WebSocket, Ws2},
};

use crate::{
    api::{self, Name},
    games::{
        game::{Broadcast, Cmd, ExpectResponse, Game, Request},
        tugofsketch::{Feedback, GameErr},
    },
};
pub use tugofsketch::Id;

type ManagerChannel = mpsc::Sender<ManagerRequest<api::GameReq>>;

quick_error! {
    #[derive(Debug)]
    enum CommError {
        CommError(loc: u32, err: mpsc::SendError<Message>) {
            display("games.rs:{}: {}", loc, err)
        }
    }
}

quick_error! {
    #[derive(Debug)]
    enum GameInteruption {
        EverybodyLeft {}
        GameError(err: GameErr) {
            from()
            display("Game: {}", err)
        }
        ReceiveChannelError(err: ()) {
            from()
            description("The oversee function's input was interrupted abruptly")
        }
        ManagerChannelError(err: mpsc::SendError<ManagerRequest<api::GameReq>>) {
            from()
        }
        CommunicationError(err: CommError) {
            from()
            display("Communication error: {}", err)
        }
    }
}

/// A message sent to a `ConnectionManager`.
#[derive(Debug)]
enum ManagerRequest<Req> {
    /// A message to be directly processed by the game
    Game(game::Request<Id, Req, Feedback>),
    /// A connection with given player `Name` has been established
    Join(Id, WebSocket),
    /// A player `Name` is joining, with an imminent connection
    Expects(Name),
}

/// The result of an attempt to add an user to a game room.
#[derive(Debug)]
pub enum ManagerResponse {
    /// The player was accepted into the room
    Accept(Id),
    /// The player wasn't accepted into the room
    Refuse,
}

struct ConnectionManager<G> {
    room_name:    String,
    connections:  SecondaryMap<Id, Connection>,
    manager_sink: ManagerChannel,
    respond:      sync::mpsc::SyncSender<ManagerResponse>,
    game:         G,
}

struct Connection(mpsc::UnboundedSender<Message>);

macro_rules! handle_cmd {
    ($manager:expr, $cmd:expr) => {
        match $cmd {
            Cmd::In(feedbacks) => {
                for (delay, feedback) in feedbacks {
                    $manager.queue(delay, feedback);
                }
                Ok(())
            }
            Cmd::Immediately(msg) => {
                loop_feedback($manager, Request::Feedback(msg))
            }
            Cmd::None => Ok(()),
        }
    };
}

fn loop_feedback<G>(
    manager: &mut ConnectionManager<G>,
    request: game::Request<Id, G::Request, Feedback>,
) -> Result<(), GameInteruption>
where
    G::Request: From<api::GameReq>,
    G: Game<Id, Error = GameErr, Response = api::GameMsg, Feedback = Feedback>,
{
    let (response, commands) = manager.game.tells(request)?;
    manager.broadcast(response)?;
    handle_cmd!(manager, commands)
}

fn oversee<G>(
    mut manager: ConnectionManager<G>,
    msg: ManagerRequest<G::Request>,
) -> Result<ConnectionManager<G>, GameInteruption>
where
    G::Request: From<api::GameReq>,
    G: Game<Id, Error = GameErr, Response = api::GameMsg, Feedback = Feedback>,
{
    use game::Request::*;
    match msg {
        ManagerRequest::Join(user, ws) => {
            let (has_joined, resp, cmds) = manager.game.joins(user)?;
            if has_joined {
                manager.broadcast(resp)?;
                handle_cmd!(&mut manager, cmds)?;
                manager.join(user, ws);
            } else {
                warn!("User joined room but wasn't expected");
            }
        }
        ManagerRequest::Game(msg) => {
            if let Leaves(id) = msg {
                let room = &manager.room_name;
                if !manager.connections.remove(id).is_some() {
                    error!("disconnect non-conn {:?}: {}", id, room);
                } else {
                    debug!("disconnected {:?}", id);
                }
            };
            loop_feedback(&mut manager, msg)?
        }
        ManagerRequest::Expects(name) => {
            match manager.game.expect(name.clone()) {
                ExpectResponse::Accept(id, cmd) => {
                    info!("Adding {} to {}", name, manager.room_name);
                    handle_cmd!(&mut manager, cmd)?;
                    manager.respond.send(ManagerResponse::Accept(id));
                }
                ExpectResponse::Refuse => {
                    warn!("{} refused {}", manager.room_name, name);
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
    manager_sink: ManagerChannel,
    recv_manager: sync::Mutex<sync::mpsc::Receiver<ManagerResponse>>,
}

impl GameRoom {
    /// Create a new empty game room.
    pub fn new(
        room_name: String,
        on_empty: impl FnOnce() + Send + 'static,
    ) -> Self {
        let (manager_sink, receiv_chan) = mpsc::channel(64);
        let (respond, recv_manager) = sync::mpsc::sync_channel(8);
        let manager = ConnectionManager {
            room_name: room_name.clone(),
            respond,
            connections: SecondaryMap::with_capacity(16),
            manager_sink: manager_sink.clone(),
            game: tugofsketch::Game::new(),
        };
        warp::spawn(
            receiv_chan
                .from_err::<GameInteruption>()
                .fold(manager, oversee)
                .then(move |conclusion| {
                    on_empty();
                    match conclusion {
                        Err(reason) => {
                            info!("Closing {} because: {}", room_name, reason);
                            Ok(())
                        }
                        Ok(_) => {
                            error!("Unreachable path at games.rs:{}", line!());
                            Err(())
                        }
                    }
                }),
        );
        GameRoom {
            manager_sink,
            recv_manager: sync::Mutex::new(recv_manager),
        }
    }

    /// Tells the game that `user` is joining. Returns how it was handled.
    pub fn expect(&mut self, user: Name) -> ManagerResponse {
        use self::ManagerRequest::Expects;
        let errmsg = "Error communicating with game manager. The game state \
                      is corrupted, there is no point keeping this game room \
                      up.";
        // TODO: verify if I can remove that clone
        let mut chan = self.manager_sink.clone();
        chan.start_send(Expects(user.clone())).expect(errmsg);
        chan.poll_complete().expect(errmsg);
        self.recv_manager.lock().unwrap().recv().expect(errmsg)
    }

    /// Tell the game to accept a given connection from `user`.
    /// Returns the server response to the connection
    pub fn accept(&mut self, user: Id, ws: Ws2) -> impl warp::reply::Reply {
        let game_chan = self.manager_sink.clone();
        debug!("accepting {:?}", user);
        ws.on_upgrade(move |socket| {
            game_chan
                .send(ManagerRequest::Join(user, socket))
                .map_err(|_| ())
                .map(|_| ())
        })
    }
}

macro_rules! comm_err {
    ($to_try:expr) => {
        ($to_try).map_err(|e| CommError::CommError(line!(), e))
    };
}
macro_rules! send {
    ($connection:expr, $message:expr) => {{
        let msg = Message::text($message.as_str());
        comm_err!($connection.start_send(msg))
    }};
}

impl<G> ConnectionManager<G>
where
    G: Game<Id>,
{
    fn queue(&mut self, delay: Duration, feedback: Feedback) {
        use self::ManagerRequest::Game;
        use game::Request::Feedback;

        let sink_to_manager = self.manager_sink.clone();
        let request = Game(Feedback(feedback));
        warp::spawn(
            sleep(delay)
                .then(move |_| sink_to_manager.send(request))
                .map(|_| ())
                .map_err(|_| ()),
        );
    }

    fn join(&mut self, id: Id, ws: WebSocket) {
        use self::ManagerRequest::Game;
        use game::Request::{Leaves, Message};
        // Split the socket into a sender and receive of messages.
        let (socket_sink, socket_stream) = ws.split();

        // Use an unbounded channel to handle buffering and flushing of
        // messages to the websocket...
        let (buffer_sink, buffer_stream) = mpsc::unbounded();
        warp::spawn(
            buffer_stream
                .inspect(|resp| debug!("ws send: {:?}", resp))
                .map_err(|()| -> warp::Error {
                    panic!("unreachable at games.rs:{}", line!())
                })
                .forward(socket_sink)
                .map(|_| ())
                .map_err(|ws_err| error!("ws send: {}", ws_err)),
        );
        self.connections.insert(id, Connection(buffer_sink));
        let client_stream = socket_stream
            .inspect(|req| debug!("ws receive: {:?}", req))
            .map(move |msg| {
                msg.to_str()
                    .map_err(|()| Error::custom("invalid string"))
                    .and_then(from_str)
                    .map(|v| Game(Message(id, v)))
                    .unwrap_or_else(|err| {
                        error!("validation: {}/ `{:?}`", err, msg.to_str());
                        Game(Leaves(id))
                    })
            });
        let sink_to_manager = self.manager_sink.clone();
        warp::spawn(
            client_stream
                .or_else(move |err| {
                    error!("ws recieve {:?}:{}", id, err);
                    Ok(Game(Leaves(id)))
                })
                .take_while(move |msg| {
                    Ok(match msg {
                        Game(Leaves(id_)) => &id != id_,
                        _ => true,
                    })
                })
                .chain(stream::once(Ok(Game(Leaves(id)))))
                .forward(sink_to_manager.sink_map_err(|_| ()))
                .map(|_| ()),
        );
    }

    fn broadcast(
        &mut self,
        targets: Broadcast<Id, api::GameMsg>,
    ) -> Result<(), CommError> {
        match targets {
            Broadcast::ToAll(msg) => self.broadcast_to_all(&msg)?,
            Broadcast::ToList(ids, message) => {
                let message: String = to_string(&message).unwrap();
                for id in ids.iter() {
                    let mut connection = &self.connections[*id].0;
                    send!(connection, message)?;
                }
                for id in ids.iter() {
                    debug!("polling {:?}", id);
                    let mut connection = &self.connections[*id].0;
                    comm_err!(connection.poll_complete())?;
                }
            }
            Broadcast::ToAllBut(id, to_all, to_other) => {
                self.broadcast_to_all_but(id, &to_all, &to_other)?;
            }
            Broadcast::ToNone => {}
        };
        Ok(())
    }

    fn broadcast_to_all_but(
        &mut self,
        except: Id,
        message: &api::GameMsg,
        other: &Option<api::GameMsg>,
    ) -> Result<(), CommError> {
        let message = to_string(message).unwrap();
        for (id, connection) in self.connections.iter_mut() {
            if id == except {
                if let Some(other) = other {
                    let message = to_string(other).unwrap();
                    send!(connection.0, message)?;
                }
            } else {
                send!(connection.0, message)?;
            }
        }
        debug!("polling all");
        for connection in self.connections.values_mut() {
            comm_err!(connection.0.poll_complete())?;
        }
        Ok(())
    }

    fn broadcast_to_all(
        &mut self,
        message: &api::GameMsg,
    ) -> Result<(), CommError> {
        let message = to_string(message).unwrap();
        for connection in self.connections.values_mut() {
            send!(connection.0, message)?;
        }
        debug!("polling all");
        for connection in self.connections.values_mut() {
            comm_err!(connection.0.poll_complete())?;
        }
        Ok(())
    }
}
