use crate::api;

/// How a game responds to a player attempting to leave it.
pub enum LeaveResponse<E> {
    /// The player left the game, but it keeps going.
    Successfully(api::Name),
    /// The player left and there is no one remaining, the game is over.
    Empty(api::Name),
    /// An attempt was made to leave the game, but an error occured
    Failed(E),
}

/// How a game responds to a player attempting to join it
pub enum JoinResponse<Id> {
    /// The player has been refused from entering the game
    Refuse,
    /// The player was accepted, and will be refered as `Id` in the future.
    Accept(Id),
}

pub enum TellResponse<Id, Msg> {
    ToAll(Msg),
    ToList(Vec<Id>, Msg),
    ToNone,
}

/// The state of a game running on Friendsketch.
/// It is capable of handling recieved messages (`Request`) and respond to them
/// (`Response`).
///
/// A `Game` has its own naming system for managing players, it is an `Id`.
/// Once a player joined, they are attributed an `Id` by the game, future calls
/// to the game will reference players using `Id`. However, **when a player
/// leaves, their name must be returned**. So it is important to keep track of
/// the player `Id` to `api::Name` relation.
pub trait Game<Id> {
    type Request;
    type Response;
    type Error;

    fn new() -> Self;

    /// A new player is joining the game room. The `Game` may choose to either
    /// `JoinResponse::Refuse` the new player or `JoinResponse::Accept(id)`,
    /// where `id` will be the identifier of the player for the rest of the
    /// game (it's an arbitrary value chosen by the `Game`).
    fn joins(&mut self, new_player: api::Name) -> JoinResponse<Id>;

    /// The existing player identified by `Id` has decided to leave the `Game`.
    fn leaves(&mut self, player: Id) -> LeaveResponse<Self::Error>;

    /// A message is sent to the `Game` by `player` identified by `Id`
    fn tells(
        &mut self,
        player: Id,
        request: Self::Request,
    ) -> Result<TellResponse<Id, Self::Response>, Self::Error>;
}
