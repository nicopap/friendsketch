pub use super::party::Id;
use super::{
    game,
    party::{Challenge, GameEnding, Guess, Party, Remove},
};
use crate::{
    api::{
        self, ChatMsg,
        GameMsg::{self, HiddenEvent, VisibleEvent},
        Name, Stroke,
    },
    words::{self, Word},
};
use arraydeque::{ArrayDeque, Wrapping};
use log::{debug, error, info, warn};
use quick_error::quick_error;
use slotmap::SecondaryMap;
use std::time::{Duration, Instant};

// debug and test config
#[cfg(debug_assertions)]
mod consts {
    use std::time::Duration;
    pub(super) const TICK_UPDATE: Duration = Duration::from_secs(10);
    pub(super) const REVEAL_INTERVAL: Duration = Duration::from_secs(10);
    pub(super) const TALLY_LENGTH: Duration = Duration::from_secs(3);
    pub(super) const DROP_DELAY: Duration = Duration::from_secs(1);
    pub(super) const JOIN_DELAY: Duration = Duration::from_secs(10);
    pub(super) const RESTART_INTERVAL: i16 = 80;
}

// release config
#[cfg(not(debug_assertions))]
mod consts {
    use std::time::Duration;
    pub(super) const TICK_UPDATE: Duration = Duration::from_secs(25);
    pub(super) const REVEAL_INTERVAL: Duration = Duration::from_secs(10);
    pub(super) const TALLY_LENGTH: Duration = Duration::from_secs(5);
    pub(super) const DROP_DELAY: Duration = Duration::from_secs(5);
    pub(super) const JOIN_DELAY: Duration = Duration::from_secs(30);
    pub(super) const RESTART_INTERVAL: i16 = 60;
}

quick_error! {
    #[derive(Debug)]
    pub enum GameErr {
        NoOneLeft {
            description("No player remaining")
        }
        BadEnd {
            description("Couldn't end the round (because it's not a round)")
        }
        BadStart {
            description("Couldn't start a new round")
        }
    }
}

enum Game_<'a> {
    Empty,
    Lobby {
        leader: Id,
    },
    /// Result screen for the previous round
    RoundResults,
    Playing {
        drawing: Vec<Stroke>,
        artist:  Id,
        word:    Word<'a>,
        lap:     Instant,
    },
    EndResults(Instant, SecondaryMap<Id, ()>),
}

/// Something that `Game` recieves back from the manager
#[derive(Debug)]
pub struct Feedback(Feedback_);

#[derive(Debug)]
enum Feedback_ {
    TickTimeout(RoundId),
    RevealLetter(RoundId),
    NextRound(RoundId),
    EndRound(RoundId),
    MakeNewMaster,
    TooLate(Id, Challenge),
    Restart(RoundId),
    CoutVotes,
}

type RoundId = (u8, u16);

pub struct Game<'a> {
    state:          Game_<'a>,
    players:        Party,
    game_log:       ArrayDeque<[api::VisibleEvent; 20], Wrapping>,
    round_duration: i16,
    start_id:       u8,
    collection:     words::Collection<'a>,
}

type Broadcast = game::Broadcast<Id, GameMsg>;
type Cmd = game::Cmd<Feedback>;

macro_rules! broadcast {
    (to_all, $msg:expr) => {
        game::Broadcast::ToAll($msg)
    };
    (to, $players:expr, $msg:expr) => {
        game::Broadcast::ToList($players.collect(), $msg)
    };
    (to_all_but, $player:expr, $msg:expr, $optmsg:expr) => {
        game::Broadcast::ToAllBut($player, $msg, $optmsg)
    };
    (to_unique, $player:expr, $msg:expr) => {
        game::Broadcast::ToList(vec![$player], $msg)
    };
    (nothing) => {
        game::Broadcast::ToNone
    };
}

/// Returns whether there are enough votes to proceed
fn count_votes(votes: &SecondaryMap<Id, ()>, party: &Party) -> bool {
    votes.len() > party.len() / 2
}

impl<'a> Game<'a> {
    fn round_challenge(&self) -> RoundId {
        (self.start_id, self.players.rounds_elapsed())
    }

    fn make_feedback(
        &self,
        to_make: impl Fn(RoundId) -> Feedback_,
    ) -> Feedback {
        Feedback(to_make((self.start_id, self.players.rounds_elapsed())))
    }

    /// Make a `GameMsg::Sync` based on the game's state
    fn msg_copy(&self, to: Option<Id>) -> GameMsg {
        use api::GameScreen::{EndSummary, Lobby, Round, Scores};
        debug!("sending scoreboard with: {}", self.players);
        let scores = if let Game_::RoundResults = self.state {
            self.players.full_standings_copy()
        } else {
            self.players.scoreboard_copy()
        };
        let history = self.game_log.iter().map(Clone::clone).collect();
        let screen = match self.state {
            Game_::Empty => {
                error!("Attempt to sync to empty state");
                panic!("tugofsketch:{} unreachable path", line!())
            }
            Game_::Lobby { leader } => Lobby {
                master: self.players.name_of(leader),
            },
            Game_::RoundResults => Scores,
            Game_::Playing {
                ref drawing,
                artist,
                lap,
                ref word,
            } => {
                let word = if let Some(player) = to {
                    if player == artist {
                        Some(api::Guess::Artist(word.to_string()))
                    } else if self.players.can_guess(player) {
                        Some(api::Guess::Guess(word.len() as u16))
                    } else {
                        None
                    }
                } else {
                    None
                };
                Round {
                    timeout: self.round_duration
                        - lap.elapsed().as_secs() as i16,
                    drawing: drawing.clone(),
                    artist: self.players.name_of(artist),
                    word,
                }
            }
            Game_::EndResults(lap, ref votes) => {
                let timeout =
                    consts::RESTART_INTERVAL - lap.elapsed().as_secs() as i16;
                let players =
                    votes.keys().map(|id| self.players.name_of(id)).collect();
                EndSummary(timeout, players)
            }
        };
        HiddenEvent(api::HiddenEvent::Sync {
            screen,
            history,
            scores,
        })
    }

    fn game_over(
        &mut self,
        mut scores: api::Scoreboard,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        self.start_id += 1;
        use self::Feedback_::Restart;
        scores.sort_unstable_by_key(|(_, s)| api::score_total(s));

        self.game_log.push_front(api::VisibleEvent::SyncComplete);
        let vote_keep = SecondaryMap::with_capacity(self.players.len());
        let lap = Instant::now();
        self.state = Game_::EndResults(lap, vote_keep);

        let timeout = consts::RESTART_INTERVAL;
        let msg = HiddenEvent(api::HiddenEvent::Complete(timeout, scores));
        let delay = Duration::from_secs(timeout as u64);
        Ok((
            broadcast!(to_all, msg),
            game::Cmd::In(delay, self.make_feedback(Restart)),
        ))
    }

    /// Initiate a new round, with new artist, new word etc
    ///
    /// If the game is over, will reset state etc.
    fn next_round(
        &mut self,
        instigator: Option<Id>,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        use self::{
            api::{
                Guess::{Artist, Guess},
                HiddenEvent::Start,
                RoundStart,
                VisibleEvent::SyncStart,
            },
            Feedback_::*,
        };
        let maybe_artist = match self.state {
            Game_::RoundResults | Game_::EndResults(..) => {
                self.players.new_round()
            }
            Game_::Lobby { leader } => match instigator {
                None => {
                    error!("Game starts itself in lobby state");
                    self.players.new_round()
                }
                Some(player) if player == leader => self.players.new_round(),
                Some(player) => {
                    warn!(
                        "player {} not leader, but tried to start game",
                        self.players.name_of(player),
                    );
                    return Ok((
                        broadcast!(
                            to_unique,
                            player,
                            self.msg_copy(Some(player))
                        ),
                        game::Cmd::None,
                    ));
                }
            },
            _ => {
                error!("Attempt to start new round in an incompatible state");
                return Err(GameErr::BadStart);
            }
        };
        let artist = match maybe_artist {
            Err(GameEnding::Complete(s)) => return self.game_over(s),
            Err(GameEnding::NoArtistLeft) => return Err(GameErr::BadStart),
            Err(GameEnding::OneRemaining(leader)) => {
                self.state = Game_::Lobby { leader };
                self.start_id += 1;
                return Ok((
                    broadcast!(to_all, self.msg_copy(None)),
                    game::Cmd::None,
                ));
            }
            Ok(x) => x,
        };
        // collection is an infinite iterator
        let word = self.collection.next().unwrap().2;
        let word_len = word.len() as u16;
        let word_rep = word.to_string();
        self.state = Game_::Playing {
            drawing: vec![],
            artist,
            word,
            lap: Instant::now(),
        };

        let artist_name = self.players.name_of(artist);
        self.game_log.push_front(SyncStart(artist_name.clone()));
        let make_msg = |word| {
            HiddenEvent(Start(RoundStart {
                timeout: self.round_duration,
                artist: artist_name.clone(),
                word,
            }))
        };
        let msg = make_msg(Guess(word_len));
        let artist_msg = make_msg(Artist(word_rep));

        let round_length = Duration::from_secs(self.round_duration as u64);
        Ok((
            broadcast!(to_all_but, artist, msg, Some(artist_msg)),
            game::Cmd::InMultiple(vec![
                (consts::TICK_UPDATE, self.make_feedback(TickTimeout)),
                (consts::REVEAL_INTERVAL, self.make_feedback(RevealLetter)),
                (round_length, self.make_feedback(EndRound)),
            ]),
        ))
    }

    /// check if player guessed the word correctly. Update game state
    /// accordingly. If the guess was correct, returns `Some(_)`, otherwise
    /// `None`. If all players have guessed, returns `Some(true)`.
    fn guesses(&mut self, player: Id, message: &[u8]) -> Option<bool> {
        use self::api::VisibleEvent::Guessed;
        if let Game_::Playing {
            artist, ref word, ..
        } = self.state
        {
            if word == *message {
                match self.players.correct(artist, player) {
                    Guess::Cannot => None,
                    Guess::Confirmed => {
                        let guesser = self.players.name_of(player);
                        self.game_log.push_front(Guessed(guesser));
                        Some(false)
                    }
                    Guess::AllGuessed => {
                        let guesser = self.players.name_of(player);
                        self.game_log.push_front(Guessed(guesser));
                        Some(true)
                    }
                }
            } else {
                None
            }
        } else {
            warn!("A guess occured outside of a regular play time");
            Some(true) // Force termination of round if there is something fishy
        }
    }

    /// End the round and broadcast the score values for that round
    fn end_round(&mut self) -> Result<(Broadcast, Cmd), GameErr> {
        use api::{HiddenEvent::Over, VisibleEvent::SyncOver};
        if let Game_::Playing { ref word, .. } = self.state {
            let word_rep = word.to_string();
            info!("Ending round {}", self.players.rounds_elapsed());
            let round_scores = self.players.current_standings();
            self.state = Game_::RoundResults;
            self.game_log.push_front(SyncOver(word_rep.clone()));
            let send = Over(word_rep, round_scores);
            let msg = self.make_feedback(Feedback_::NextRound);
            let cmd = game::Cmd::In(consts::TALLY_LENGTH, msg);
            Ok((broadcast!(to_all, HiddenEvent(send)), cmd))
        } else {
            error!("Ending a round that is not happening");
            Err(GameErr::BadEnd)
        }
    }

    fn remove(
        &mut self,
        removed: Id,
        challenge: Challenge,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        use api::VisibleEvent::Left;
        let immediately = |msg| game::Cmd::Immediately(Feedback(msg));
        match self.players.remove(removed, challenge) {
            Remove::EmptyParty => Err(GameErr::NoOneLeft),
            Remove::OneRemaining(name, leader) => {
                let left = VisibleEvent(Left(name.clone()));
                self.game_log.push_front(Left(name));
                self.state = Game_::Lobby { leader };
                self.start_id += 1;
                let cmd = immediately(Feedback_::MakeNewMaster);
                Ok((broadcast!(to_all, left), cmd))
            }
            Remove::Failed => {
                info!("{:?} wasn't removed, since challenge invalid", removed);
                Ok((broadcast!(nothing), game::Cmd::None))
            }
            Remove::AllGuessed(name) | Remove::WasArtist(name) => {
                debug!(
                    "Ending round because {} was the last to guess/the artist",
                    name,
                );
                let left = VisibleEvent(Left(name.clone()));
                self.game_log.push_front(Left(name));
                let command = match self.state {
                    Game_::Lobby { leader } if leader == removed => {
                        immediately(Feedback_::MakeNewMaster)
                    }
                    Game_::EndResults(_, ref mut votes) => {
                        votes.remove(removed);
                        immediately(Feedback_::CoutVotes)
                    }
                    Game_::Playing { artist, .. } => {
                        if artist == removed {
                            debug!("It was the artist");
                        }
                        game::Cmd::Immediately(
                            self.make_feedback(Feedback_::EndRound),
                        )
                    }
                    _ => {
                        debug!("Round-ending leave outside of play, fine");
                        game::Cmd::None
                    }
                };
                Ok((broadcast!(to_all, left), command))
            }
            Remove::Ok(name) => {
                let left = VisibleEvent(Left(name.clone()));
                self.game_log.push_front(Left(name));
                let command = match self.state {
                    Game_::Lobby { leader } if leader == removed => {
                        immediately(Feedback_::MakeNewMaster)
                    }
                    Game_::EndResults(_, ref mut votes) => {
                        votes.remove(removed);
                        immediately(Feedback_::CoutVotes)
                    }
                    _ => game::Cmd::None,
                };
                Ok((broadcast!(to_all, left), command))
            }
        }
    }

    fn respond(
        &mut self,
        player: Id,
        request: api::GameReq,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        use self::api::GameReq;
        match request {
            GameReq::Sync => Ok((
                broadcast!(to_unique, player, self.msg_copy(Some(player))),
                game::Cmd::None,
            )),
            GameReq::Start => match self.state {
                Game_::Lobby { .. } => self.next_round(Some(player)),
                Game_::EndResults(_, ref mut votes) => {
                    votes.insert(player, ());
                    debug!("{} voted", self.players.name_of(player));
                    if count_votes(votes, &self.players) {
                        self.next_round(None)
                    } else {
                        let name = self.players.name_of(player);
                        let msg = VisibleEvent(api::VisibleEvent::Voted(name));
                        Ok((broadcast!(to_all, msg), game::Cmd::None))
                    }
                }
                _ => Ok((
                    broadcast!(to_unique, player, self.msg_copy(Some(player))),
                    game::Cmd::None,
                )),
            },
            GameReq::Canvas(msg) => match self.state {
                Game_::Playing {
                    ref mut drawing,
                    artist,
                    ..
                } if artist == player => {
                    use self::api::CanvasMsg;
                    match msg {
                        CanvasMsg::Start(point, ref color, size) => {
                            let new_stroke =
                                Stroke(point, vec![], color.clone(), size);
                            drawing.push(new_stroke);
                        }
                        CanvasMsg::Continue(point) => {
                            drawing.last_mut().unwrap().1.push(point)
                        }
                        CanvasMsg::End => {}
                    }
                    let msg = GameMsg::Canvas(msg);
                    Ok((
                        broadcast!(to_all_but, artist, msg, None),
                        game::Cmd::None,
                    ))
                }
                _ => Ok((
                    broadcast!(to_unique, player, self.msg_copy(Some(player))),
                    game::Cmd::None,
                )),
            },
            GameReq::Chat(msg) => match self.state {
                Game_::Playing { ref word, .. } => {
                    use self::{game::Cmd, Feedback_::EndRound};
                    use api::{HiddenEvent::Correct, VisibleEvent::Guessed};
                    let word_rep = word.to_string();
                    if let Some(complete) = self.guesses(player, msg.as_bytes())
                    {
                        let guesser = self.players.name_of(player);
                        let msg = VisibleEvent(Guessed(guesser));
                        let correct = HiddenEvent(Correct(word_rep));
                        let cmd = if complete {
                            let end_round = self.make_feedback(EndRound);
                            Cmd::Immediately(end_round)
                        } else {
                            Cmd::None
                        };
                        Ok((
                            broadcast!(to_all_but, player, msg, Some(correct)),
                            cmd,
                        ))
                    } else {
                        let final_msg = api::VisibleEvent::Message(ChatMsg {
                            content: msg,
                            author:  self.players.name_of(player),
                        });
                        self.game_log.push_front(final_msg.clone());
                        Ok((
                            broadcast!(to_all, VisibleEvent(final_msg)),
                            Cmd::None,
                        ))
                    }
                }
                _ => {
                    let final_msg = api::VisibleEvent::Message(ChatMsg {
                        content: msg,
                        author:  self.players.name_of(player),
                    });
                    self.game_log.push_front(final_msg.clone());
                    Ok((
                        broadcast!(to_all, VisibleEvent(final_msg)),
                        game::Cmd::None,
                    ))
                }
            },
        }
    }

    fn feedback(
        &mut self,
        Feedback(msg): Feedback,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        use self::{
            api::HiddenEvent::{Mastery, Reveal, TimeoutSync},
            Feedback_::*,
            Game_::*,
        };
        let cr = self.round_challenge();
        match (&mut self.state, msg) {
            (_, TooLate(id, challenge)) => self.remove(id, challenge),
            (Lobby { ref mut leader }, MakeNewMaster) => {
                let id = self.players.a_player().ok_or(GameErr::NoOneLeft)?;
                *leader = id;
                let msg = HiddenEvent(Mastery);
                Ok((broadcast!(to_unique, id, msg), game::Cmd::None))
            }
            (Playing { word, artist, .. }, RevealLetter(r)) if r == cr => {
                if let Some((index, letter)) = word.next_letter() {
                    let artist = *artist;
                    let msg = HiddenEvent(Reveal(index, letter));
                    let cmd = self.make_feedback(RevealLetter);
                    Ok((
                        broadcast!(to_all_but, artist, msg, None),
                        game::Cmd::In(consts::REVEAL_INTERVAL, cmd),
                    ))
                } else {
                    Ok((broadcast!(nothing), game::Cmd::None))
                }
            }
            (Playing { .. }, EndRound(r)) if r == cr => self.end_round(),
            (Playing { lap, .. }, TickTimeout(r)) if r == cr => {
                let timeout =
                    self.round_duration - lap.elapsed().as_secs() as i16;
                if timeout <= 0 {
                    self.end_round()
                } else {
                    let send = TimeoutSync(timeout);
                    let msg = self.make_feedback(TickTimeout);
                    let cmd = game::Cmd::In(consts::TICK_UPDATE, msg);
                    Ok((broadcast!(to_all, HiddenEvent(send)), cmd))
                }
            }
            (RoundResults, NextRound(r)) if r == cr => self.next_round(None),
            (EndResults(..), Restart(r)) if r == cr => self.next_round(None),
            (EndResults(_, ref votes), CoutVotes) => {
                if count_votes(votes, &self.players) {
                    self.next_round(None)
                } else {
                    Ok((broadcast!(nothing), game::Cmd::None))
                }
            }
            msg @ (RoundResults, TickTimeout(_))
            | msg @ (Playing { .. }, TickTimeout(_))
            | msg @ (RoundResults, EndRound(_))
            | msg @ (Playing { .. }, EndRound(_))
            | msg @ (RoundResults, RevealLetter(_))
            | msg @ (Playing { .. }, RevealLetter(_)) => {
                debug!("Expected outdated message: {:?}", msg.1);
                Ok((broadcast!(nothing), game::Cmd::None))
            }
            (_state, feedback) => {
                warn!("invalid feedback: {:?} for game state", feedback);
                Ok((broadcast!(nothing), game::Cmd::None))
            }
        }
    }

    pub fn new(
        round_duration: i16,
        set_count: u8,
        collection: words::Collection<'a>,
    ) -> Self {
        Game {
            state: Game_::Empty,
            players: Party::new(0, set_count),
            game_log: ArrayDeque::new(),
            start_id: 0,
            round_duration,
            collection,
        }
    }
}
impl<'a> game::Game<Id> for Game<'a> {
    type Error = GameErr;
    type Feedback = Feedback;
    type Request = api::GameReq;
    type Response = GameMsg;

    fn expect(
        &mut self,
        name: Name,
    ) -> Result<(Option<Id>, Broadcast, Cmd), GameErr> {
        use self::Feedback_::TooLate;
        use api::VisibleEvent::Joined;
        if let Some((id, drop_id)) = self.players.expect(name.clone()) {
            let cmd = game::Cmd::In(
                consts::JOIN_DELAY,
                Feedback(TooLate(id, drop_id)),
            );
            if let Game_::Empty = self.state {
                self.state = Game_::Lobby { leader: id };
            };
            self.game_log.push_front(Joined(name.clone()));
            let msg = VisibleEvent(Joined(name));
            Ok((Some(id), broadcast!(to_all, msg), cmd))
        } else {
            Ok((None, broadcast!(nothing), game::Cmd::None))
        }
    }

    fn joins(&mut self, player: Id) -> Result<(bool, Broadcast, Cmd), GameErr> {
        let cmd = if let Game_::EndResults(_, ref mut votes) = self.state {
            votes.insert(player, ());
            game::Cmd::Immediately(Feedback(Feedback_::CoutVotes))
        } else {
            game::Cmd::None
        };
        Ok((self.players.joins(player), broadcast!(nothing), cmd))
    }

    fn tells(
        &mut self,
        request: game::Request<Id, api::GameReq, Feedback>,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        match request {
            game::Request::Leaves(id) => {
                let drop_id = self.players.drop(id);
                let cmd = game::Cmd::In(
                    consts::DROP_DELAY,
                    Feedback(Feedback_::TooLate(id, drop_id)),
                );
                Ok((broadcast!(nothing), cmd))
            }
            game::Request::Feedback(feedback) => self.feedback(feedback),
            game::Request::Message(id, msg) => self.respond(id, msg),
        }
    }
}
