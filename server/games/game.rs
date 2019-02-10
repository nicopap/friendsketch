use crate::api;
use std::time::Duration;

#[derive(Debug)]
pub enum Broadcast<Id, Msg> {
    ToAll(Msg),
    ToList(Vec<Id>, Msg),
    /// Send `Msg` to all but `Id`. send `Option<Msg>` to `Id` if `Some`
    ToAllBut(Id, Msg, Option<Msg>),
    ToNone,
}

/// Order the game manager to do the following effectfull actions
#[derive(Debug)]
pub enum Cmd<Msg> {
    /// Send back to `Game` given `Msg` in `Duration`
    In(Duration, Msg),
    /// Send back to `Game` given `Msg` in `Duration` (multiple times)
    InMultiple(Vec<(Duration, Msg)>),
    /// update `Game` immediately with given `Msg`
    Immediately(Msg),
    /// Do nothing effectfull
    None,
}

#[derive(Debug)]
pub enum Request<Id, Msg, Feedback> {
    /// The existing player identified by `Id` has decided to leave the `Game`.
    Leaves(Id),
    /// React to a self-sent `Request`
    Feedback(Feedback),
    /// A message is sent to the `Game` by `player` identified by `Id`
    Message(Id, Msg),
}

pub type GameResponse<Id, Resp, Feedback, Error> =
    Result<(Broadcast<Id, Resp>, Cmd<Feedback>), Error>;

/// The state of a game running on Friendsketch.
/// It is capable of handling recieved messages (`Request`) and respond to them
/// (`Response`).
///
/// A `Game` has its own naming system for managing players, it is an `Id`.
/// Once a player joined, they are attributed an `Id` by the game, future calls
/// to the game will reference players using `Id`.
pub trait Game<Id: slotmap::Key> {
    type Request;
    type Response;
    type Feedback;
    type Error;

    fn new() -> Self;

    /// A new player is expected to join the game room. The `Game` may choose
    /// to either refuse the player and return `None` or accept the player and
    /// return `Some(id)` where `id` will be the identifier of the player for
    /// the rest of the game (it's an arbitrary value chosen by the `Game`).
    fn expect(
        &mut self,
        new_player: api::Name,
    ) -> Result<
        (
            Option<Id>,
            Broadcast<Id, Self::Response>,
            Cmd<Self::Feedback>,
        ),
        Self::Error,
    >;

    /// A new player attempts to join the game. Returns whether the game
    /// accepted the player or not.
    fn joins(
        &mut self,
        player: Id,
    ) -> Result<
        (bool, Broadcast<Id, Self::Response>, Cmd<Self::Feedback>),
        Self::Error,
    >;

    fn tells(
        &mut self,
        request: Request<Id, Self::Request, Self::Feedback>,
    ) -> GameResponse<Id, Self::Response, Self::Feedback, Self::Error>;
}
