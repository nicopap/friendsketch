#![feature(uniform_paths)]
mod api;
mod games;
mod join;

use bytes::buf::Buf;
use std::sync::Arc;

use chashmap::CHashMap;
use log::{debug, info, warn};
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
    games::GameRoom,
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

macro_rules! respond {
    ($status:expr, $body:expr, $($header:tt)*) => ({
        let mut response = Response::builder();
        respond!(@ response $($header)*)
            .status($status)
            .body($body)
            .map_err(|e| panic!("unreachable '{:?}' at main.rs:{}", e, line!()))
    });
    (@ $val:ident none) => ($val);
    (@ $val:ident json) => ($val.header("Content-Type","text/json"));
    (@ $val:ident html) => ($val.header("Content-Type","text/html"));
    (@ $val:ident Location($loc:expr)) => ($val.header("Location",$loc));
}

fn handle_join_room(
    join_id: join::Uid,
    manager: Arc<JoinManager>,
) -> Result<impl Reply, Rejection> {
    let (ok, not_found) = (StatusCode::OK, StatusCode::NOT_FOUND);
    if let Some(roomid) = manager.remove(&join_id) {
        respond!(ok, api::pages::JoinRoomBody(roomid).to_string(), html)
    } else {
        respond!(not_found, api::pages::JoinRoomErr.to_string(), html)
    }
}

fn handle_join_forms(
    roomid: api::RoomId,
    manager: Arc<JoinManager>,
    server: Server,
) -> Result<impl Reply, Rejection> {
    if server.get(&roomid).is_some() {
        let new_uid = join::Uid::new(&mut rand::thread_rng());
        let uid_location = {
            let mut stem = String::from("*/");
            stem.push_str(Into::<String>::into(&new_uid).as_str());
            stem
        };
        manager.insert(new_uid, roomid);
        respond!(StatusCode::SEE_OTHER, "", Location(uid_location))
    } else {
        respond!(StatusCode::NOT_FOUND, api::pages::JoinFormErr.into(), html)
    }
}

fn handle_create(
    api::CreateReq { username, .. }: api::CreateReq,
    server: Server,
) -> Result<impl Reply, Rejection> {
    // TODO: guarenteed to cause collision of room names at one point
    // checking that the roomid isn't already taken would be ideal
    let roomid = RoomId::new_random();
    let room_name = format!("{}", roomid);
    let sanitized_room_name = format!("\"{}\"", room_name);
    let server_ref = server.clone();
    let roomid_copy = roomid.clone();
    let on_empty = move || {
        debug!("Removing {} from server", &roomid_copy);
        server_ref.remove(&roomid_copy);
    };
    info!("Room created: {} by user {}", room_name, username);
    server.insert(roomid, GameRoom::new(room_name, on_empty));
    respond!(StatusCode::CREATED, sanitized_room_name, json)
}

fn handle_join(
    api::JoinReq { roomid, username }: api::JoinReq,
    server: Server,
) -> Result<impl Reply, Rejection> {
    use games::ManagerResponse::{Accept, Refuse};
    match server.get_mut(&roomid) {
        Some(mut room) => match room.expect(username) {
            Accept => respond!(StatusCode::OK, "\"classic\"", json),
            Refuse => respond!(StatusCode::CONFLICT, "", none),
        },
        None => respond!(StatusCode::NOT_FOUND, "", none),
    }
}

fn handle_report(report: body::FullBody) -> Result<impl Reply, Rejection> {
    let report_message = String::from_utf8_lossy(report.bytes());
    warn!("{}", report_message);
    respond!(StatusCode::OK, "", none)
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
