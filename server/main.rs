mod api;
mod games;

use std::{collections::BTreeSet, sync::Arc};

use chashmap::CHashMap;
use log::{info, warn};
use pretty_env_logger;
use warp::{
    self,
    filters::ws::Ws2,
    http::{response::Response, StatusCode},
    path, Filter,
};

use self::{
    api::{Name, RoomId},
    games::GameManager,
};

type ServerState = CHashMap<RoomId, Room>;

/// Manage what players has access to a specific instance of a game party.
/// One must first be "expected" to then be "accepted" into the game.
/// A `Room` wraps a `GameManager`.
struct Room {
    newcomings: BTreeSet<Name>,
    presents:   BTreeSet<Name>, // TODO: use GameManager user management system
    game:       GameManager,
}

type Server = Arc<ServerState>;

impl Room {
    /// Create a new empty room.
    fn new(room_name: String) -> Self {
        Room {
            game:       GameManager::new(room_name),
            newcomings: BTreeSet::new(),
            presents:   BTreeSet::new(),
        }
    }

    /// Add `user` to the list of people to expect
    /// returns an `Err` if the user was already present in the expected list
    /// or is present in the room
    fn expect(&mut self, user: Name) -> Result<(), ()> {
        if (!self.presents.contains(&user)) && self.newcomings.insert(user) {
            Ok(())
        } else {
            Err(())
        }
    }

    /// If `accepted` is in the list of expected users, returns the GameManager
    /// and validated name, necessary to accept the connection,
    /// Otherwise returns `Err`.
    fn accept(&mut self, accepted: Name) -> Option<(GameManager, Name)> {
        if self.newcomings.remove(&accepted) {
            self.presents.insert(accepted.clone());
            let game = self.game.clone();
            Some((game, accepted))
        } else {
            None
        }
    }
}

fn main() {
    pretty_env_logger::init();

    let server = Arc::new(ServerState::new());
    let server_ref = warp::any().map(move || server.clone());

    macro_rules! json_body {
        () => {
            warp::body::content_length_limit(1024 * 16).and(warp::body::json())
        };
    };

    let room_path = warp::post2().and(path!("friendk" / "rooms"));

    let room_join = room_path
        .and(path("join"))
        .and(path::end())
        .and(json_body!())
        .and(server_ref.clone())
        .and_then(handle_join);

    let room_create = room_path
        .and(path("create"))
        .and(path::end())
        .and(json_body!())
        .and(server_ref.clone())
        .and_then(handle_create);

    let websockets = path!("friendk" / "ws" / RoomId / Name)
        .and(warp::ws2())
        .and(server_ref.clone())
        .and_then(accept_conn);

    let routes = websockets
        .or(room_join)
        .or(room_create)
        .with(warp::log("friendsketch"));
    warp::serve(routes).run(([127, 0, 0, 1], 8073));
}

fn handle_create(
    api::CreateReq { username, .. }: api::CreateReq,
    server: Server,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut response = Response::builder();
    // TODO: guarenteed to cause collision of room names at one point
    // checking that the roomid isn't already taken would be ideal
    let roomid = RoomId::new_random();
    let room_name = format!("{}", roomid);
    let sanitized_room_name = format!("\"{}\"", &room_name);
    info!("Room created: {} by user {}", &room_name, &username);
    server.insert(roomid, Room::new(room_name));
    response
        .status(StatusCode::CREATED)
        .header("Content-Type", "text/json")
        .body(sanitized_room_name)
        .map_err(|_| unreachable!())
}

fn handle_join(
    api::JoinReq { roomid, username }: api::JoinReq,
    server: Server,
) -> Result<impl warp::Reply, warp::Rejection> {
    let result = (move || -> Result<_, StatusCode> {
        let logmsg = format!("User {} is now expected to join", &username);
        // room exists
        let mut room = server.get_mut(&roomid).ok_or(StatusCode::NOT_FOUND)?;
        // name is not already taken
        room.expect(username).or(Err(StatusCode::CONFLICT))?;
        info!("{}", logmsg);
        let mut response = Response::builder();
        Ok(response
            .status(StatusCode::OK)
            .header("Content-Type", "text/json")
            .body("\"pintclone\"")
            .map_err(|_| unreachable!()))
    })();
    result.unwrap_or_else(|code| {
        warn!("Join failed with status code: {}", &code);
        let mut response = Response::builder();
        response.status(code).body("").map_err(|_| unreachable!())
    })
}

fn accept_conn(
    roomid: RoomId,
    name: api::Name,
    ws: Ws2,
    server: Server,
) -> Result<impl warp::reply::Reply, warp::Rejection> {
    let url = format!("/ws/{}/{}", &roomid, &name);
    let result = (move || {
        // room exists
        let mut room = server.get_mut(&roomid)?;
        // name is expected to join & accept connection
        let (game, name) = room.accept(name)?;
        Some(ws.on_upgrade(move |socket| game.join(name, socket)))
    })();
    result.ok_or_else(|| {
        warn!("Rejected connection to {}", &url);
        warp::reject::not_found()
    })
}
