#![feature(uniform_paths)]
mod api;
mod games;
mod join;
mod server;

use bytes::buf::Buf;
use std::sync::Arc;

use chashmap::CHashMap;
use log::warn;
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
    api::{pages, RoomId},
    server::ServerState,
};

type JoinManager = CHashMap<join::Uid, RoomId>;

type Server = Arc<ServerState>;

macro_rules! b {
    ($body_type:ident) => {
        warp::body::content_length_limit(1024 * 16).and(body::$body_type())
    };
}

fn main() {
    use self::{join::Uid, server::ConnId};
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
    let join_form = path!("friendk" / "join" / "*" / Uid)
        .and(join_manager_ref.clone())
        .and_then(handle_join_form);

    let join_redirect = url! {
        ("friendk" / "join" / RoomId),
        join_manager_ref.clone(),
        handle_join_redirect,
    };

    let join = url! {("friendk"/"rooms"/"join"), b!(json), handle_join};
    let create = url! {("friendk"/"rooms"/"create"), b!(json), handle_create};
    let ws_url = url! {("friendk"/"ws"/ConnId), warp::ws2(), accept_conn};
    let report = path!("friendk" / "report")
        .and(b!(concat))
        .and_then(handle_report);

    let routes = ws_url
        .or(join)
        .or(create)
        .or(report)
        .or(join_redirect)
        .or(join_form);

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
    (@ $val:ident text) => ($val.header("Content-Type","text/plain"));
    (@ $val:ident Location($loc:expr)) => ($val.header("Location",$loc));
}

fn handle_join_form(
    join_id: join::Uid,
    manager: Arc<JoinManager>,
) -> Result<impl Reply, Rejection> {
    let (ok, not_found) = (StatusCode::OK, StatusCode::NOT_FOUND);
    if let Some(roomid) = manager.remove(&join_id) {
        respond!(ok, pages::JoinFormBody(roomid).to_string(), html)
    } else {
        respond!(not_found, pages::JoinFormErr.to_string(), html)
    }
}

fn handle_join_redirect(
    roomid: api::RoomId,
    manager: Arc<JoinManager>,
    server: Server,
) -> Result<impl Reply, Rejection> {
    if server.is_active(&roomid) {
        let new_uid = join::Uid::new(&mut rand::thread_rng());
        let uid_location = {
            let mut stem = String::from("*/");
            stem.push_str(Into::<String>::into(&new_uid).as_str());
            stem
        };
        manager.insert(new_uid, roomid);
        respond!(StatusCode::SEE_OTHER, "", Location(uid_location))
    } else {
        respond!(StatusCode::NOT_FOUND, pages::JoinRedirectErr.into(), html)
    }
}

fn handle_create(
    api::CreateReq { .. }: api::CreateReq,
    server: Server,
) -> Result<impl Reply, Rejection> {
    let roomid = format!("{}", server.create_room());
    respond!(StatusCode::CREATED, roomid, text)
}

fn handle_join(
    api::JoinReq { roomid, username }: api::JoinReq,
    server: Server,
) -> Result<impl Reply, Rejection> {
    use server::ExpectFailure::{NotFound, Refuse};
    match server.expect(roomid, username) {
        Ok(accepted) => respond!(StatusCode::OK, accepted.into_url(), text),
        Err(Refuse) => respond!(StatusCode::CONFLICT, String::new(), none),
        Err(NotFound) => respond!(StatusCode::NOT_FOUND, String::new(), none),
    }
}

fn handle_report(report: body::FullBody) -> Result<impl Reply, Rejection> {
    let report_message = String::from_utf8_lossy(report.bytes());
    warn!("{}", report_message);
    respond!(StatusCode::OK, "", none)
}

fn accept_conn(
    id: server::ConnId,
    ws: Ws2,
    server: Server,
) -> Result<impl Reply, Rejection> {
    server.connect(id, ws).map_err(|err| {
        warn!("Connection to {:?} failed with: {}", id, err);
        warp::reject::not_found()
    })
}
