use crate::api;

pub enum JoinResponse<Id> {
    Refuse,
    Accept(Id),
}

/// The state of a game running on Friendsketch.
/// It is capable of handling recieved messages (`Request`) and respond to them
/// (`Response`).
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

    /// The existing player identified by `Id` has decided to leave the `Game`
    fn leaves(
        &mut self,
        player: Id,
    ) -> Result<(api::Name, Vec<(Id, Self::Response)>), Self::Error>;

    /// A message is sent to the `Game` by `player` identified by `Id`
    fn tells(
        &mut self,
        player: Id,
        request: Self::Request,
    ) -> Result<Vec<(Id, Self::Response)>, Self::Error>;
}
