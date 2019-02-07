#![feature(uniform_paths)]
mod api;
mod games;
mod server;

use bytes::buf::Buf;
use std::sync::Arc;

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
    api::{pages, RoomId},
    server::{ServerState, UrlId},
};

type Server = Arc<ServerState>;

macro_rules! b {
    ($body_type:ident) => {
        warp::body::content_length_limit(1024 * 16).and(body::$body_type())
    };
}

fn main() {
    use self::server::{ConnId, JoinId};
    pretty_env_logger::init();

    let server = Arc::new(ServerState::new());
    let server_ref = warp::any().map(move || server.clone());

    macro_rules! url {
        {($($path:tt)*), $handler:ident $(, $body:expr)?} => (
            path!($($path)*)
                $(.and($body))?
                .and(server_ref.clone())
                .and_then($handler)
        )
    }
    let join_form = url! {("friendk"/"join"/"*"/JoinId), handle_join_form};
    let join_redirect = url! {("friendk"/"join"/RoomId), handle_join_redirect};

    let join = url! {("friendk"/"rooms"/"join"), handle_join, b!(json)};
    let create = url! {("friendk"/"rooms"/"create"), handle_create, b!(json)};
    let ws_url = url! {("friendk"/"ws"/ConnId), accept_conn, warp::ws2()};
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
    join_id: server::JoinId,
    server: Server,
) -> Result<impl Reply, Rejection> {
    let (ok, not_found) = (StatusCode::OK, StatusCode::NOT_FOUND);
    if let Some(roomid) = server.consume(join_id) {
        respond!(ok, pages::JoinFormBody(roomid).to_string(), html)
    } else {
        warn!("Attempt to access outdated room link");
        respond!(not_found, pages::JoinFormErr.to_string(), html)
    }
}

fn handle_join_redirect(
    roomid: api::RoomId,
    server: Server,
) -> Result<impl Reply, Rejection> {
    if let Some(new_uid) = server.generate(roomid) {
        let uid_location = {
            let mut stem = String::from("*/");
            stem.push_str(&new_uid.into_url());
            stem
        };
        info!("Successfully redirect to {}", uid_location);
        respond!(StatusCode::SEE_OTHER, "", Location(uid_location))
    } else {
        info!("Attempt to access innexistant room");
        respond!(StatusCode::NOT_FOUND, pages::JoinRedirectErr.into(), html)
    }
}

fn handle_create(
    _: api::CreateReq,
    server: Server,
) -> Result<impl Reply, Rejection> {
    respond!(StatusCode::CREATED, server.create_room().to_string(), text)
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
