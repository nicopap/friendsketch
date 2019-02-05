#![feature(uniform_paths)]
mod api;
mod games;
mod join;

use bytes::buf::Buf;
use std::sync::Arc;

use chashmap::CHashMap;
use log::{info, warn};
use pretty_env_logger;
use warp::{
    self, body,
    filters::ws::Ws2,
    http::{response::Response, StatusCode},
    path,
    reply::Reply,
    Filter, Rejection,
};

use self::{
    api::{Name, RoomId},
    games::{GameRoom, ManagerResponse},
};

type ServerState = CHashMap<RoomId, GameRoom>;
type JoinManager = CHashMap<join::Uid, RoomId>;

type Server = Arc<ServerState>;

macro_rules! b {
    ($body_type:ident) => {
        warp::body::content_length_limit(1024 * 16).and(body::$body_type())
    };
}

fn main() {
    use self::join::Uid;
    pretty_env_logger::init();

    let join_manager = Arc::new(JoinManager::new());
    let join_manager_ref = warp::any().map(move || join_manager.clone());
    let server = Arc::new(ServerState::new());
    let server_ref = warp::any().map(move || server.clone());

    macro_rules! url {
        {($($path:tt)*), $body:expr, $handler:expr $(,)?} => (
            path!($($path)*)
                .and($body)
                .and(server_ref.clone())
                .and_then($handler)
        )
    }
    let join_room = path!("friendk" / "join" / "*" / Uid)
        .and(join_manager_ref.clone())
        .and_then(handle_join_room);

    let join_form = url! {
        ("friendk" / "join" / RoomId),
        join_manager_ref.clone(),
        handle_join_forms,
    };

    let join = url! {("friendk"/"rooms"/"join"), b!(json), handle_join};
    let create = url! {("friendk"/"rooms"/"create"), b!(json), handle_create};
    let ws_url = url! {("friendk"/"ws"/RoomId/Name), warp::ws2(), accept_conn};
    let report = path!("friendk" / "report")
        .and(b!(concat))
        .and_then(handle_report);

    let routes = ws_url
        .or(join)
        .or(create)
        .or(report)
        .or(join_form)
        .or(join_room);
    warp::serve(routes).run(([127, 0, 0, 1], 8073));
}

fn handle_join_room(
    join_id: join::Uid,
    manager: Arc<JoinManager>,
) -> Result<impl Reply, Rejection> {
    let mut response = Response::builder();
    if let Some(roomid) = manager.remove(&join_id) {
        response
            .status(StatusCode::OK)
            .header("Content-Type", "text/html")
            .body(Into::<String>::into(api::pages::JoinRoomBody(roomid)))
            .map_err(|e| panic!("unreachable '{:?}' at main.rs:{}", e, line!()))
    } else {
        response
            .status(StatusCode::NOT_FOUND)
            .header("Content-Type", "text/html")
            .body(Into::<&str>::into(api::pages::JoinRoomErr).to_string())
            .map_err(|e| panic!("unreachable '{:?}' at main.rs:{}", e, line!()))
    }
}

fn handle_join_forms(
    roomid: api::RoomId,
    manager: Arc<JoinManager>,
    server: Server,
) -> Result<impl Reply, Rejection> {
    let mut response = Response::builder();
    if server.get(&roomid).is_some() {
        let new_uid = join::Uid::new(&mut rand::thread_rng());
        let uid_location = {
            let mut stem = String::from("*/");
            stem.push_str(Into::<String>::into(&new_uid).as_str());
            stem
        };
        manager.insert(new_uid, roomid);
        response
            .status(StatusCode::SEE_OTHER)
            .header("Location", uid_location)
            .body("")
            .map_err(|e| panic!("unreachable '{:?}' at main.rs:{}", e, line!()))
    } else {
        response
            .status(StatusCode::NOT_FOUND)
            .header("Content-Type", "text/html")
            .body(api::pages::JoinFormErr.into())
            .map_err(|e| panic!("unreachable '{:?}' at main.rs:{}", e, line!()))
    }
}

fn handle_create(
    api::CreateReq { username, .. }: api::CreateReq,
    server: Server,
) -> Result<impl Reply, Rejection> {
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
        .map_err(|e| panic!("unreachable '{:?}' at main.rs:{}", e, line!()))
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
) -> Result<impl Reply, Rejection> {
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
                    .body("\"classic\"")
                    .map_err(|e| {
                        panic!("unreachable '{:?}' at main.rs:{}", e, line!())
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
        .map_err(|e| panic!("unreachable '{:?}' at main.rs:{}", e, line!()))
}

fn handle_report(report: body::FullBody) -> Result<impl Reply, Rejection> {
    let report_message = String::from_utf8_lossy(report.bytes());
    warn!("{}", report_message);
    let mut response = Response::builder();
    response
        .status(StatusCode::OK)
        .body("")
        .map_err(|e| panic!("unreachable '{:?}' at main.rs:{}", e, line!()))
}

fn accept_conn(
    roomid: RoomId,
    name: api::Name,
    ws: Ws2,
    server: Server,
) -> Result<impl Reply, Rejection> {
    let url = format!("/ws/{}/{}", roomid, name);
    // room exists
    let mut room = server.get_mut(&roomid).ok_or_else(|| {
        warn!("Rejected connection to {}", url);
        warp::reject::not_found()
    })?;
    // name is expected to join & accept connection
    Ok(room.accept(name, ws))
}
