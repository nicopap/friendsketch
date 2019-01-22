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
    games::GameRoom,
};

type ServerState = CHashMap<RoomId, GameRoom>;

type Server = Arc<ServerState>;

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
    server.insert(roomid, GameRoom::new(room_name));
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
    // room exists
    let mut room = server.get_mut(&roomid).ok_or_else(|| {
        warn!("Rejected connection to {}", &url);
        warp::reject::not_found()
    })?;
    // name is expected to join & accept connection
    Ok(room.accept(name, ws))
}
