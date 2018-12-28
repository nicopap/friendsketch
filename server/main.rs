mod api;
mod games;
mod roomids;

use std::{collections::BTreeSet, sync::Arc};

use chashmap::CHashMap;
use log::{info, warn};
use pretty_env_logger;
use serde::Deserialize;
use warp::{
    self,
    filters::ws::Ws2,
    fs::dir,
    http::{response::Response, StatusCode},
    path, reply, Filter,
};

use self::{api::Name, games::GameManager, roomids::RoomId};

type ServerState = CHashMap<RoomId, Room>;

/// Manage what players has access to a specific instance of a game party.
/// One must first be "expected" to then be "accepted" into the game.
/// A `Room` wraps a `GameManager`.
struct Room {
    newcomings: BTreeSet<Name>,
    presents:   BTreeSet<Name>,
    game:       GameManager,
}

type Server = Arc<ServerState>;

#[derive(Debug, Deserialize)]
struct JoinReq {
    roomid:   String,
    username: String,
}

#[derive(Debug, Deserialize)]
struct CreateReq {
    username: String,
    game:     String,
}

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

    let root = path::end().map(reply).map(|r| {
        let r = reply::with_header(r, "Location", "/lobby/index.html");
        reply::with_status(r, StatusCode::FOUND)
    });
    macro_rules! json_body {
        () => {
            warp::body::content_length_limit(1024 * 16).and(warp::body::json())
        };
    };

    let room_path = warp::post2().and(path("rooms"));

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

    let chat = path!("ws" / String / String)
        .and(warp::ws2())
        .and(server_ref.clone())
        .and_then(accept_conn);

    let routes = root
        .or(room_join)
        .or(room_create)
        .or(chat)
        .or(dir("build"))
        .with(warp::log("friendsketch"));
    warp::serve(routes).run(([127, 0, 0, 1], 8080));
}

fn handle_create(
    CreateReq { username, .. }: CreateReq,
    server: Server,
) -> Result<impl warp::Reply, warp::Rejection> {
    let mut response = Response::builder();
    match api::Name::try_from(username) {
        Ok(valid) => {
            // TODO: guarenteed to cause collision of room names at one point
            // checking that the roomid isn't already taken would be ideal
            let roomid = roomids::gen();
            let mut room_name = String::with_capacity(32);
            room_name.push('"');
            let actual_name: String = (&roomid).into();
            room_name.push_str(&actual_name);
            room_name.push('"');
            info!("Room created: {} by user {}", &actual_name, &valid);
            server.insert(roomid, Room::new(actual_name));
            response
                .status(StatusCode::CREATED)
                .header("Content-Type", "text/json")
                .body(room_name)
                .map_err(|_| unreachable!())
        }
        Err(_) => {
            warn!("user with malformed name attempted to create a room");
            response
                .status(StatusCode::BAD_REQUEST)
                .body("".to_owned())
                .map_err(|_| unreachable!())
        }
    }
}

fn handle_join(
    JoinReq { roomid, username }: JoinReq,
    srv: Server,
) -> Result<impl warp::Reply, warp::Rejection> {
    let result = (move || -> Result<_, StatusCode> {
        // room is a valid value
        let valid = RoomId::try_from(&roomid).ok_or(StatusCode::NOT_FOUND)?;
        // name is valid
        let name =
            Name::try_from(username).or(Err(StatusCode::BAD_REQUEST))?;
        let logmsg = format!("User {} is now expected to join", &name);
        // room exists
        let mut room = srv.get_mut(&valid).ok_or(StatusCode::NOT_FOUND)?;
        // name is not already taken (TODO: figure out how to handle leavers)
        room.expect(name).or(Err(StatusCode::CONFLICT))?;
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
    roomid: String,
    name: String,
    ws: Ws2,
    server: Server,
) -> Result<impl warp::reply::Reply, warp::Rejection> {
    let url = format!("/ws/{}/{}", &roomid, &name);
    let result = (move || {
        // room is a valid value
        let validated = RoomId::try_from(&roomid)?;
        // room exists
        let mut room = server.get_mut(&validated)?;
        // name is valid
        let name = Name::try_from(name).ok()?;
        // name is expected to join & accept connection
        let (game, name) = room.accept(name)?;
        Some(ws.on_upgrade(move |socket| game.join(name, socket)))
    })();
    result.ok_or_else(|| {
        warn!("Rejected connection to {}", &url);
        warp::reject::not_found()
    })
}
