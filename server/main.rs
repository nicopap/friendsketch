#![feature(uniform_paths)]
mod api;
mod games;

use std::sync::Arc;

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
    games::{GameRoom, ManagerResponse},
};

type ServerState = CHashMap<RoomId, GameRoom>;

type Server = Arc<ServerState>;

fn main() {
    pretty_env_logger::init();

    let server = Arc::new(ServerState::new());
    let server_ref = warp::any().map(move || server.clone());

    macro_rules! json {
        () => {
            warp::body::content_length_limit(1024 * 16).and(warp::body::json())
        };
    };
    macro_rules! url {
        {($($path:tt)*), $body:expr, $handler:expr $(,)?} => (
            path!($($path)*)
                .and($body)
                .and(server_ref.clone())
                .and_then($handler)
        )
    }
    let join = url! {("friendk"/"rooms"/"join"), json!(), handle_join};
    let create = url! {("friendk"/"rooms"/"create"), json!(), handle_create};
    let ws_url = url! {("friendk"/"ws"/RoomId/Name), warp::ws2(), accept_conn};

    let routes = ws_url.or(join).or(create).with(warp::log("friendsketch"));
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
    let sanitized_room_name = format!("\"{}\"", room_name);
    info!("Room created: {} by user {}", room_name, username);
    server.insert(roomid, GameRoom::new(room_name));
    response
        .status(StatusCode::CREATED)
        .header("Content-Type", "text/json")
        .body(sanitized_room_name)
        .map_err(|_| panic!("unreachable at {}:{}", module_path!(), line!()))
}

enum Handle {
    NoRoom,
    EmptyRoom,
    Refuse,
    Accept,
}
fn handle_join(
    api::JoinReq { roomid, username }: api::JoinReq,
    server: Server,
) -> Result<impl warp::Reply, warp::Rejection> {
    let err_code = {
        let logmsg = format!("User {} is now expected to join", &username);
        let handle = match server.get_mut(&roomid) {
            Some(mut room) => match room.expect(username) {
                ManagerResponse::Accept => Handle::Accept,
                ManagerResponse::Refuse => Handle::Refuse,
                ManagerResponse::Empty => Handle::EmptyRoom,
            },
            None => Handle::NoRoom,
        };
        match handle {
            Handle::NoRoom => StatusCode::NOT_FOUND,
            Handle::Accept => {
                info!("{}", logmsg);
                return Response::builder()
                    .status(StatusCode::OK)
                    .header("Content-Type", "text/json")
                    .body("\"pintclone\"")
                    .map_err(|_| {
                        panic!("unreachable at {}:{}", module_path!(), line!())
                    });
            }
            Handle::Refuse => StatusCode::CONFLICT,
            Handle::EmptyRoom => {
                server.remove(&roomid);
                StatusCode::NOT_FOUND
            }
        }
    };
    warn!("Join failed with status code: {}", err_code);
    let mut response = Response::builder();
    response
        .status(err_code)
        .body("")
        .map_err(|_| panic!("unreachable at {}:{}", module_path!(), line!()))
}

fn accept_conn(
    roomid: RoomId,
    name: api::Name,
    ws: Ws2,
    server: Server,
) -> Result<impl warp::reply::Reply, warp::Rejection> {
    let url = format!("/ws/{}/{}", roomid, name);
    // room exists
    let mut room = server.get_mut(&roomid).ok_or_else(|| {
        warn!("Rejected connection to {}", url);
        warp::reject::not_found()
    })?;
    // name is expected to join & accept connection
    Ok(room.accept(name, ws))
}
