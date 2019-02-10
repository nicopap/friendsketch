//! Handle players in a game of friendsketch
//!
//! The `Party` struct keeps track of the current round and the current set.
//! Please refer to the design doc for definition of a "set" and a "round".

use super::scores::ScoreKeeper;
use crate::api::{self, Name};
use log::{debug, error, info};
use slotmap::{new_key_type, SecondaryMap, SlotMap};
use std::{fmt, iter::repeat};

#[cfg_attr(test, derive(PartialEq, Debug))]
pub enum Guess {
    /// Player cannot guess
    Cannot,
    /// All players have guessed
    AllGuessed,
    /// The player can guess and their score have been updated
    Confirmed,
}

#[cfg_attr(test, derive(PartialEq, Debug))]
pub enum Remove {
    /// The player has been removed
    Ok(Name),
    /// The player has been removed and was the artist
    WasArtist(Name),
    /// The player has been removed and there is no one left to guess
    AllGuessed(Name),
    /// The player was removed, and there is no one remaining
    EmptyParty,
    /// The player isn't in "expecting" state anymore, impossible to remove
    Failed,
}

new_key_type! { pub struct Id; }

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Challenge(u16);

#[derive(Clone, Debug)]
enum RoundScore {
    Failed,
    Absent,
    Artist(u16),
    Guessed(u16),
}
impl Into<api::RoundScore> for RoundScore {
    fn into(self) -> api::RoundScore {
        use self::RoundScore::*;
        match self {
            Failed => api::RoundScore::Failed,
            Absent => api::RoundScore::Absent,
            Artist(score) => api::RoundScore::Artist(score),
            Guessed(score) => api::RoundScore::Guessed(score),
        }
    }
}

impl fmt::Display for RoundScore {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::RoundScore::*;
        match self {
            Absent => f.pad("--"),
            Artist(score) => {
                let score_str = format!("*{}", score);
                f.pad(&score_str)
            }
            Guessed(score) => f.pad(&score.to_string()),
            Failed => f.pad("0"),
        }
    }
}

#[derive(Clone, Debug)]
struct Player {
    name: Name,
    score: Vec<RoundScore>,
    drop_id: Option<Challenge>,
    /// Whether the player was the artist during this set
    set_drawn: bool,
}

impl Player {
    fn new(name: Name, elapsed: usize) -> (Player, Challenge) {
        let challenge = Challenge(rand::random());
        let elapsed = elapsed.saturating_sub(1);
        let player = Player {
            name,
            drop_id: Some(challenge),
            score: repeat(RoundScore::Absent).take(elapsed).collect(),
            set_drawn: false,
        };
        (player, challenge)
    }

    fn drop(&mut self) -> Challenge {
        let new_challenge = Challenge(rand::random());
        self.drop_id = Some(new_challenge);
        new_challenge
    }
}

impl Into<(Name, Vec<api::RoundScore>)> for Player {
    fn into(self) -> (Name, Vec<api::RoundScore>) {
        let Player { name, score, .. } = self;
        let score = score.into_iter().map(Into::into).collect();
        (name, score)
    }
}

#[derive(Debug)]
pub struct Party {
    players:        SlotMap<Id, Player>,
    current_set:    u8,
    rounds_elapsed: u16,
    round_scores:   SecondaryMap<Id, RoundScore>,
    score_keeper:   ScoreKeeper,
    game_started:   bool,
}

impl fmt::Display for Party {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "round {} at set {}\n",
            self.rounds_elapsed, self.current_set
        )?;
        for (id, player) in self.players.iter() {
            write!(f, "{:>20}: ", player.name)?;
            for score in player.score.iter() {
                write!(f, "{:<6}", score)?;
            }
            if let Some(curr_score) = self.round_scores.get(id) {
                write!(f, "+ {}", curr_score)?;
            };
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl Party {
    /// Creates a new party, with given capacity
    ///
    /// If 0 is given, will default to 8.
    pub fn new(capacity: usize) -> Party {
        let capacity = if capacity == 0 { 8 } else { capacity };
        Party {
            players:        SlotMap::with_capacity_and_key(capacity),
            current_set:    1,
            rounds_elapsed: 0,
            round_scores:   SecondaryMap::with_capacity(capacity),
            score_keeper:   ScoreKeeper::new(capacity as u16),
            game_started:   false,
        }
    }

    pub fn rounds_elapsed(&self) -> u16 {
        self.rounds_elapsed
    }

    /// Returns id of a player if they are the only one in the game currently
    pub fn one_remaining(&self) -> Option<Id> {
        if self.players.len() == 1 {
            self.players.keys().next()
        } else {
            None
        }
    }

    /// Returns a copy of the name of given player
    ///
    /// Panics if `id` is not a player
    pub fn name_of(&self, id: Id) -> Name {
        self.players[id].name.clone()
    }

    /// Enter a new round
    ///
    /// Returns which player is the artist this round.
    /// If all players already has drawn this set, we pass to the next set and
    /// return the first artist for this set.
    /// Finally, if despite getting to a new set, we can't find an appropriate
    /// player to be the artist, returns `None`
    pub fn new_round(&mut self) -> Option<Id> {
        use self::RoundScore::Failed;
        self.rounds_elapsed += 1;
        self.score_keeper = ScoreKeeper::new(self.players.len() as u16);
        if self.game_started {
            for (id, player) in self.players.iter_mut() {
                // TODO: think about what to do of drop_id.is_none()
                let score = self.round_scores.remove(id).unwrap_or(Failed);
                player.score.push(score);
            }
        } else {
            self.game_started = true;
        }
        self.round_scores.clear();
        info!("Entering round {}", self.rounds_elapsed);
        debug!("{}", self);
        let players = &mut self.players;
        let round_scores = &mut self.round_scores;
        let current_set = &mut self.current_set;

        macro_rules! create_artist {
            () => {
                players.iter_mut().find_map(|(id, player)| {
                    if !player.set_drawn && player.drop_id.is_none() {
                        player.set_drawn = true;
                        round_scores.insert(id, RoundScore::Artist(0));
                        Some(id)
                    } else {
                        None
                    }
                })
            };
        };
        create_artist!().or_else(|| {
            *current_set += 1;
            info!("Entering set {}", current_set);
            players.iter_mut().for_each(|(_, p)| p.set_drawn = false);
            create_artist!()
        })
    }

    /// Sets an "expected" player as having joined
    ///
    /// Returns whether the player was expected.
    pub fn joins(&mut self, player: Id) -> bool {
        self.players
            .get_mut(player)
            .map(|x| x.drop_id = None)
            .is_some()
    }

    /// Insert a new player into the party
    ///
    /// The player start as "expected", which means they won't be selected as
    /// artist until they `joins`.
    ///
    /// Returns `None` if a player with a similar name is already in the game,
    /// otherwise returns the `Id` by which the player will be referenced in
    /// the future, and the `Challenge` necessary to invalidate the player if
    /// they do not join in time.
    pub fn expect(&mut self, name: Name) -> Option<(Id, Challenge)> {
        let same_name = self
            .players
            .values()
            .any(|Player { name: n, .. }| *n == name);
        if same_name {
            None
        } else {
            let (new_player, challenge) =
                Player::new(name, self.rounds_elapsed as usize);
            let new_id = self.players.insert(new_player);
            self.round_scores.insert(new_id, RoundScore::Absent);
            Some((new_id, challenge))
        }
    }

    /// Sets an existing player as "expected"
    ///
    /// Returns the `Challenge` necessary to confirm that it has indeed left.
    pub fn drop(&mut self, player: Id) -> Challenge {
        self.players[player].drop()
    }

    /// Remove for good a player from the game
    ///
    /// Will not remove them if the challenge fails.
    pub fn remove(&mut self, id: Id, chal: Challenge) -> Remove {
        use self::{Remove::*, RoundScore::Artist};
        if Some(chal) == self.players.get(id).and_then(|x| x.drop_id) {
            // self.players.get(id) => Some(...)
            let left = self.players.remove(id).unwrap().name;
            if self.players.is_empty() {
                EmptyParty
            } else if let Some(Artist(_)) = self.round_scores.get(id) {
                WasArtist(left)
            } else if self.all_guessed() {
                AllGuessed(left)
            } else {
                Ok(left)
            }
        } else {
            Failed
        }
    }

    /// Resets the game to an initial state without any score
    pub fn game_reset(&mut self) -> api::Scoreboard {
        self.new_round();
        let mut ret = Vec::with_capacity(self.players.len());
        self.players.iter_mut().for_each(|(_, player)| {
            let score = player.score.drain(..).map(Into::into).collect();
            ret.push((player.name.clone(), score));
            player.set_drawn = false;
        });
        self.game_started = false;
        self.round_scores.clear();
        self.current_set = 1;
        self.rounds_elapsed = 0;
        ret
    }

    /// Returns the scores for the current round
    pub fn current_standings(&self) -> Vec<(Name, api::RoundScore)> {
        self.players
            .iter()
            .map(|(id, player)| {
                let score = self
                    .round_scores
                    .get(id)
                    .map(|x| x.clone().into())
                    .unwrap_or(api::RoundScore::Failed);
                let name = player.name.clone();
                (name, score)
            })
            .collect()
    }

    /// Updates score such as given player guessed correctly
    pub fn correct(&mut self, artist: Id, player: Id) -> Guess {
        use self::RoundScore::{Artist, Guessed};
        if !self.round_scores.contains_key(player) {
            let score = self.score_keeper.next();
            self.round_scores.insert(player, Guessed(score));

            if let Artist(ref mut artist_score) = self.round_scores[artist] {
                *artist_score += score;
            } else {
                error!("The artist score was not properly set");
                self.round_scores.insert(artist, Artist(score));
            }
            if self.all_guessed() {
                Guess::AllGuessed
            } else {
                Guess::Confirmed
            }
        } else {
            Guess::Cannot
        }
    }

    fn all_guessed(&self) -> bool {
        self.players
            .keys()
            .all(|id| self.round_scores.contains_key(id))
    }

    /// Returns any player, if there is any
    pub fn a_player(&self) -> Option<Id> {
        self.players
            .iter()
            .filter(|x| x.1.drop_id.is_none())
            .next()
            .map(|x| x.0)
    }

    pub fn scoreboard_copy(&self) -> api::Scoreboard {
        self.players.values().map(|x| x.clone().into()).collect()
    }
}

#[cfg(test)]
#[rustfmt::skip]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;
    use std::collections::HashMap;

    struct TestContext {
        party: Party,
        name_ids: HashMap<&'static str,Id>,
        artist: Option<Id>,
    }
    impl TestContext {
        fn new() -> TestContext {
            TestContext {
                party: Party::new(0),
                name_ids: HashMap::new(),
                artist:None,
            }
        }
        fn join(&mut self, name: &'static str) -> Option<bool> {
            let name_string = api::name_from_string(name.to_string());
            let (id, _) = self.party.expect(name_string)?;
            self.name_ids.insert(name,id);
            Some(self.party.joins(id))
        }
        fn leave(&mut self, name: &'static str) -> Remove {
            let player = self.name_ids.get(name).unwrap();
            let chal = self.party.drop(*player);
            self.party.remove(*player, chal)
        }
        fn start(&mut self) -> Name {
            let id = self.party.new_round().unwrap();
            self.artist = Some(id);
            self.party.name_of(id)
        }
        fn correct(&mut self, name: &'static str) -> Guess {
            let artist = self.artist.unwrap();
            let id = self.name_ids.get(name).unwrap();
            self.party.correct(artist, *id)
        }
    }

    #[test]
    fn test_artist_leaves() {
        let mut c = TestContext::new();
        c.join("a"); c.join("b"); c.join("c"); c.join("d");
        let artist = c.start();
        assert_eq!(Remove::WasArtist(artist), c.leave("a"));
    }

    #[test]
    fn test_all_guessed_and_leave() {
        let mut c = TestContext::new();
        c.join("a"); c.join("b"); c.join("c"); c.join("d");
        c.start();
        c.correct("b"); c.correct("d");
        let c_name = api::name_from_string("c".to_string());
        assert_eq!(Remove::AllGuessed(c_name), c.leave("c"));
    }

    #[test]
    fn test_guesser_leave_and_all_guess() {
        let mut c = TestContext::new();
        c.join("a"); c.join("b"); c.join("c"); c.join("d");
        c.start();
        c.correct("b"); c.correct("d");
        let b_name = api::name_from_string("b".to_string());
        assert_eq!(Remove::Ok(b_name), c.leave("b"));
        assert_eq!(Guess::AllGuessed, c.correct("c"));
    }
    #[test]
    fn test_guesser_leaves_and_other_leaves() {
        let mut c = TestContext::new();
        c.join("a"); c.join("b"); c.join("c"); c.join("d");
        c.start();
        c.correct("b"); c.correct("d");
        let d_name = api::name_from_string("d".to_string());
        assert_eq!(Remove::Ok(d_name), c.leave("d"));
        let b_name = api::name_from_string("b".to_string());
        assert_eq!(Remove::Ok(b_name), c.leave("b"));
        let c_name = api::name_from_string("c".to_string());
        assert_eq!(Remove::AllGuessed(c_name), c.leave("c"));
        assert_eq!(c.artist, c.party.one_remaining());
    }
    #[test]
    fn test_joins_midgame1() {
        let mut c = TestContext::new();
        c.join("a"); c.join("b"); c.join("c");
        c.start();
        c.correct("b");
        c.join("d");
        assert_eq!(Guess::AllGuessed, c.correct("c"));
    }
    #[test]
    fn test_joins_midgame2() {
        let mut c = TestContext::new();
        c.join("a"); c.join("b"); c.join("c");
        c.start();
        c.correct("b");
        c.join("d");
        c.leave("d");
        assert_eq!(Guess::AllGuessed, c.correct("c"));
    }
}
