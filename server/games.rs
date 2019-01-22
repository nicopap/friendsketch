mod game;
mod sketchfighters;

use fnv::FnvHashMap;
use futures::{stream, sync::mpsc, Future, Sink, Stream};
use fxhash::FxHashMap;
use log::{debug, error, info, warn};
use serde_json::{de::from_slice, ser::to_string};
use std::{sync, time::Duration};
use tokio_timer::sleep;
use warp::{
    self,
    ws::{Message, WebSocket, Ws2},
};

use crate::{
    api::{self, Name},
    games::{
        game::{Game, JoinResponse},
        sketchfighters::Id,
    },
};

/// A message sent to a `ConnectionManager`.
#[derive(Debug)]
enum ManagerRequest {
    Msg(Id, api::GameReq),
    Terminate(Id),
    Join(Name, WebSocket),
    Expects(Name),
    GiveUp(Name),
}

pub enum ManagerResponse {
    Accepted,
    Refused,
    ManagerEmpty,
}

struct ConnectionManager<G> {
    room_name:   String,
    connections: FnvHashMap<Id, Connection>,
    client_chan: mpsc::Sender<ManagerRequest>,
    respond:     sync::mpsc::SyncSender<ManagerResponse>,
    newcomings:  FxHashMap<Name, Id>,
    game:        G,
}

struct Connection(mpsc::UnboundedSender<api::GameMsg>);

fn oversee<G: Game<Id, Error = ()>>(
    mut manager: ConnectionManager<G>,
    msg: ManagerRequest,
) -> Result<ConnectionManager<G>, ()>
where
    G::Response: Into<api::GameMsg> + Clone,
    G::Request: From<api::GameReq>,
{
    use self::api::{GameMsg, InfoMsg};
    match msg {
        ManagerRequest::Join(name, ws) => {
            if let Some(id) = manager.newcomings.remove(&name) {
                info!("Connection with {} established", &name);
                manager.join(id, ws);
            } else {
                warn!("Unexpected player {} attempted to connect", &name);
            }
        }
        ManagerRequest::Msg(id, msg_val) => {
            let response = manager.game.tells(id, msg_val.into())?;
            manager.broadcast(&response);
        }
        ManagerRequest::Terminate(id) => {
            match manager.game.leaves(id) {
                Ok((name, response)) => {
                    if let None = manager.connections.remove(&id) {
                        error!(
                            "{} attempts to leave {}, yet they aren't in it",
                            &name, &manager.room_name
                        )
                    } else {
                        info!("{} left {}", name, &manager.room_name)
                    };
                    manager.broadcast(&response);
                }
                Err(()) => {
                    warn!(
                        "Attempt to terminate innexistant {:?} in {}",
                        id, &manager.room_name
                    );
                }
            };
        }
        ManagerRequest::Expects(name) => {
            match manager.game.joins(name.clone()) {
                JoinResponse::Accept(id) => {
                    info!("Adding {} to {}", &name, &manager.room_name);
                    manager.broadcast_to_all(GameMsg::Info(InfoMsg::Joined(
                        name.clone(),
                    )));
                    manager.newcomings.insert(name, id);
                    manager.respond.send(ManagerResponse::Accepted);
                }
                JoinResponse::Refuse => {
                    warn!("{} refused {}", &manager.room_name, &name);
                    manager.respond.send(ManagerResponse::Refused);
                }
            }
        }
        ManagerRequest::GiveUp(name) => {
            if let Some(id) = manager.newcomings.remove(&name) {
                match manager.game.leaves(id) {
                    Ok((name, response)) => {
                        info!(
                            "canceled {} connection's to {}",
                            name, &manager.room_name
                        );
                        manager.broadcast(&response);
                    }
                    Err(()) => {
                        warn!(
                            "Failed to cancel {}'s connection to {}",
                            name, &manager.room_name
                        );
                    }
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
        let (send_manager, receiv_chan) = mpsc::channel(64);
        let (respond, recv_manager) = sync::mpsc::sync_channel(8);
        let manager = ConnectionManager {
            room_name,
            respond,
            connections: FnvHashMap::with_capacity_and_hasher(
                16,
                Default::default(),
            ),
            client_chan: send_manager.clone(),
            game: sketchfighters::Game::new(),
            newcomings: FxHashMap::default(),
        };
        warp::spawn(receiv_chan.fold(manager, oversee).map(|_| ()));
        GameRoom {
            send_manager,
            recv_manager: sync::Mutex::new(recv_manager),
        }
    }

    /// Add `user` to the list of people to expect
    /// returns an `Err` if the user was already present in the expected list
    /// or is present in the room
    pub fn expect(&mut self, user: Name) -> Result<(), ()> {
        let mut send_chan = self.send_manager.clone();
        send_chan.start_send(ManagerRequest::Expects(user.clone()));
        send_chan.poll_complete();
        // FIXME: expect(X) -> join(X) -> leaves(X) -> expect(X) -> Giveup(X)
        //           v---> wait 10 seconds ----------------------^
        warp::spawn(
            sleep(Duration::new(10, 0))
                .then(move |_| send_chan.send(ManagerRequest::GiveUp(user)))
                .map(|_| ())
                .map_err(|_| ()),
        );
        if let Ok(response) = self.recv_manager.lock().unwrap().recv() {
            match response {
                ManagerResponse::Accepted => Ok(()),
                ManagerResponse::Refused => Err(()),
                ManagerResponse::ManagerEmpty => Err(()),
            }
        } else {
            panic!(
                "Error communicating with game manager. The game state is \
                 corrupted, there is no point keeping this game room up."
            )
        }
    }

    /// If `accepted` is in the list of expected users, accept the connection
    /// to the websocket `ws` and returns `Some(Reply)`. Otherwise, `None`
    pub fn accept(
        &mut self,
        accepted: Name,
        ws: Ws2,
    ) -> impl warp::reply::Reply {
        let game_chan = self.send_manager.clone();
        ws.on_upgrade(move |socket| {
            game_chan
                .send(ManagerRequest::Join(accepted, socket))
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
                .chain(stream::once(Ok(ManagerRequest::Terminate(id))))
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
