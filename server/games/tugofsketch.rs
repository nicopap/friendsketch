pub use super::party::Id;
use super::{
    game,
    party::{Challenge, GameEnding, Guess, Party, Remove},
};
use crate::api::{self, ChatMsg, GameMsg, Name, Stroke, VisibleEvent};
use arraydeque::{ArrayDeque, Wrapping};
use log::{error, info, warn};
use quick_error::quick_error;
use rand::seq::IteratorRandom;
use std::time::{Duration, Instant};

// debug and test config
#[cfg(debug_assertions)]
mod consts {
    use std::time::Duration;
    pub(super) const TICK_UPDATE: Duration = Duration::from_secs(10);
    pub(super) const REVEAL_INTERVAL: Duration = Duration::from_secs(10);
    pub(super) const TALLY_LENGTH: Duration = Duration::from_secs(6);
    pub(super) const DROP_DELAY: Duration = Duration::from_secs(1);
    pub(super) const JOIN_DELAY: Duration = Duration::from_secs(10);
    pub(super) const RESTART_INTERVAL: Duration = Duration::from_secs(10);
}

// release config
#[cfg(not(debug_assertions))]
mod consts {
    use std::time::Duration;
    pub(super) const TICK_UPDATE: Duration = Duration::from_secs(25);
    pub(super) const REVEAL_INTERVAL: Duration = Duration::from_secs(10);
    pub(super) const TALLY_LENGTH: Duration = Duration::from_secs(3);
    pub(super) const DROP_DELAY: Duration = Duration::from_secs(4);
    pub(super) const JOIN_DELAY: Duration = Duration::from_secs(30);
    pub(super) const RESTART_INTERVAL: Duration = Duration::from_secs(20);
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

enum Game_ {
    Empty,
    Lobby {
        leader: Id,
    },
    /// Result screen for the previous round
    RoundResults,
    Playing {
        drawing: Vec<Stroke>,
        artist:  Id,
        word:    &'static str,
        lap:     Instant,
    },
    EndResults,
}

/// Something that `Game` recieves back from the manager
#[derive(Debug)]
pub struct Feedback(Feedback_);

#[derive(Debug)]
enum Feedback_ {
    TickTimeout(u16),
    RevealLetter(u16),
    NextRound(u16),
    EndRound(u16),
    MakeNewMaster,
    TooLate(Id, Challenge),
    Restart(u16),
}

pub struct Game {
    state:          Game_,
    players:        Party,
    game_log:       ArrayDeque<[VisibleEvent; 20], Wrapping>,
    round_duration: i16,
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

impl Game {
    /// Make a `GameMsg::Sync` based on the game's state
    fn msg_copy(&self, to: Option<Id>) -> GameMsg {
        use api::{
            GameMsg::HiddenEvent,
            GameScreen::{EndSummary, Lobby, Round, Scores},
        };
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
                word,
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
            Game_::EndResults => EndSummary,
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
        use self::Feedback_::Restart;
        scores.sort_unstable_by_key(|(_, s)| api::score_total(s));

        self.game_log.push_front(api::VisibleEvent::SyncComplete);
        self.state = Game_::EndResults;

        let msg = GameMsg::HiddenEvent(api::HiddenEvent::Complete(scores));
        let cr = self.players.rounds_elapsed();
        Ok((
            broadcast!(to_all, msg),
            game::Cmd::In(consts::RESTART_INTERVAL, Feedback(Restart(cr))),
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
                GameMsg::HiddenEvent,
                Guess::{Artist, Guess},
                HiddenEvent::Start,
                RoundStart,
                VisibleEvent::SyncStart,
            },
            Feedback_::*,
        };
        let maybe_artist = match self.state {
            Game_::RoundResults | Game_::EndResults => self.players.new_round(),
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
                return Ok((
                    broadcast!(to_all, self.msg_copy(None)),
                    game::Cmd::None,
                ));
            }
            Ok(x) => x,
        };
        let word = WORD_LIST[rand::random::<u8>() as usize];
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
        let msg = make_msg(Guess(word.len() as u16));
        let artist_msg = make_msg(Artist(word.to_string()));

        let cr = self.players.rounds_elapsed();
        let round_length = Duration::from_secs(self.round_duration as u64);
        Ok((
            broadcast!(to_all_but, artist, msg, Some(artist_msg)),
            game::Cmd::InMultiple(vec![
                (consts::TICK_UPDATE, Feedback(TickTimeout(cr))),
                (consts::REVEAL_INTERVAL, Feedback(RevealLetter(cr))),
                (round_length, Feedback(EndRound(cr))),
            ]),
        ))
    }

    /// check if player guessed the word correctly. Update game state
    /// accordingly. If the guess was correct, returns `Some(_)`, otherwise
    /// `None`. If all players have guessed, returns `Some(true)`.
    fn guesses(&mut self, player: Id, message: &[u8]) -> Option<bool> {
        use self::api::VisibleEvent::Guessed;
        if let Game_::Playing { artist, word, .. } = self.state {
            if word.as_bytes() == message {
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
        use api::{
            GameMsg::HiddenEvent, HiddenEvent::Over, VisibleEvent::SyncOver,
        };
        if let Game_::Playing { word, .. } = self.state {
            info!("Ending round {}", self.players.rounds_elapsed());
            let round_scores = self.players.current_standings();
            self.state = Game_::RoundResults;
            self.game_log.push_front(SyncOver(word.to_string()));
            let send = Over(word.to_string(), round_scores);
            let msg = Feedback_::NextRound(self.players.rounds_elapsed());
            let cmd = game::Cmd::In(consts::TALLY_LENGTH, Feedback(msg));
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
                let left = GameMsg::VisibleEvent(Left(name.clone()));
                self.game_log.push_front(Left(name));
                self.state = Game_::Lobby { leader };
                Ok((broadcast!(to_all, left), game::Cmd::None))
            }
            Remove::Failed => {
                info!("{:?} wasn't removed, since challenge invalid", removed);
                Ok((broadcast!(nothing), game::Cmd::None))
            }
            Remove::AllGuessed(name) | Remove::WasArtist(name) => {
                let left = GameMsg::VisibleEvent(Left(name.clone()));
                self.game_log.push_front(Left(name));
                let cr = self.players.rounds_elapsed();
                let cmd = immediately(Feedback_::EndRound(cr));
                Ok((broadcast!(to_all, left), cmd))
            }
            Remove::Ok(name) => {
                let left = GameMsg::VisibleEvent(Left(name.clone()));
                self.game_log.push_front(Left(name));
                let command = match self.state {
                    Game_::Lobby { leader } if leader == removed => {
                        immediately(Feedback_::MakeNewMaster)
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
            GameReq::Start => self.next_round(Some(player)),
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
                Game_::Playing { word, .. } => {
                    use self::{game::Cmd, Feedback_::EndRound};
                    use api::{
                        GameMsg::{HiddenEvent, VisibleEvent},
                        HiddenEvent::Correct,
                        VisibleEvent::Guessed,
                    };
                    if let Some(complete) = self.guesses(player, msg.as_bytes())
                    {
                        let guesser = self.players.name_of(player);
                        let msg = VisibleEvent(Guessed(guesser));
                        let correct = HiddenEvent(Correct(word.to_string()));
                        let cmd = if complete {
                            let cr = self.players.rounds_elapsed();
                            let end_round = Feedback(EndRound(cr));
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
                        broadcast!(to_all, GameMsg::VisibleEvent(final_msg)),
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
            api::{
                GameMsg::HiddenEvent,
                HiddenEvent::{Mastery, Reveal, TimeoutSync},
            },
            Feedback_::*,
            Game_::*,
        };
        let cr = self.players.rounds_elapsed();
        match (&mut self.state, msg) {
            (_, TooLate(id, challenge)) => self.remove(id, challenge),
            (Lobby { ref mut leader }, MakeNewMaster) => {
                let id = self.players.a_player().ok_or(GameErr::NoOneLeft)?;
                *leader = id;
                let msg = HiddenEvent(Mastery);
                Ok((broadcast!(to_unique, id, msg), game::Cmd::None))
            }
            (Playing { word, artist, .. }, RevealLetter(r)) if r == cr => {
                let mut rng = rand::thread_rng();
                let (index, letter) =
                    word.chars().enumerate().choose(&mut rng).unwrap();
                let msg = HiddenEvent(Reveal(index, letter));
                let cmd = Feedback(RevealLetter(cr));
                Ok((
                    broadcast!(to_all_but, *artist, msg, None),
                    game::Cmd::In(consts::REVEAL_INTERVAL, cmd),
                ))
            }
            (Playing { .. }, EndRound(r)) if r == cr => self.end_round(),
            (Playing { lap, .. }, TickTimeout(r)) if r == cr => {
                let timeout =
                    self.round_duration - lap.elapsed().as_secs() as i16;
                if timeout <= 0 {
                    self.end_round()
                } else {
                    let send = TimeoutSync(timeout);
                    let msg = TickTimeout(cr);
                    let cmd = game::Cmd::In(consts::TICK_UPDATE, Feedback(msg));
                    Ok((broadcast!(to_all, HiddenEvent(send)), cmd))
                }
            }
            (RoundResults, NextRound(r)) if r == cr => self.next_round(None),
            (EndResults, Restart(r)) if r == cr => self.next_round(None),
            (RoundResults, TickTimeout(_))
            | (Playing { .. }, TickTimeout(_))
            | (RoundResults, EndRound(_))
            | (Playing { .. }, EndRound(_))
            | (RoundResults, RevealLetter(_))
            | (Playing { .. }, RevealLetter(_)) => {
                Ok((broadcast!(nothing), game::Cmd::None))
            }
            (_state, feedback) => {
                warn!("invalid feedback: {:?} for game state", feedback);
                Ok((broadcast!(nothing), game::Cmd::None))
            }
        }
    }

    pub fn new(round_duration: i16, set_count: u8) -> Self {
        Game {
            state: Game_::Empty,
            players: Party::new(0, set_count),
            game_log: ArrayDeque::new(),
            round_duration,
        }
    }
}
impl game::Game<Id> for Game {
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
            let msg = GameMsg::VisibleEvent(Joined(name));
            Ok((Some(id), broadcast!(to_all, msg), cmd))
        } else {
            Ok((None, broadcast!(nothing), game::Cmd::None))
        }
    }

    fn joins(&mut self, player: Id) -> Result<(bool, Broadcast, Cmd), GameErr> {
        Ok((
            self.players.joins(player),
            broadcast!(nothing),
            game::Cmd::None,
        ))
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

#[rustfmt::skip]
static WORD_LIST: [&str; 256] = [ "Aardvark", "Albatross", "Alligator", "Alpaca", "Ant", "Anteater", "Antelope", "Ape", "Armadillo", "Baboon", "Badger", "Barracuda", "Bat", "Bear", "Beaver", "Bee", "Beetle", "Bird", "Bison", "Boar", "Bobcat", "Buffalo", "Bull", "Butterfly", "Camel", "Caribou", "Cassowary", "Cat", "Caterpillar", "Cattle", "Chameleon", "Chamois", "Cheetah", "Chicken", "Chimpanzee", "Chinchilla", "Chipmunk", "Chough", "Civet", "Coati", "Cobra", "Cockroach", "Cod", "Cormorant", "Cougar", "Cow", "Coyote", "Crab", "Crane", "Crocodile", "Crow", "Cuckoo", "Curlew", "Deer", "Dinosaur", "Doe", "Dog", "Dogfish", "Dolphin", "Donkey", "Dotterel", "Dove", "Dragonfly", "Dromedary", "Duck", "Dugong", "Dunlin", "Eagle", "Echidna", "Eel", "Eland", "Elephant", "ElephantSeal", "Elk", "Emu", "Falcon", "Ferret", "Finch", "Fish", "Flamingo", "Fly", "Fox", "Frog", "Gaur", "Gazelle", "Gerbil", "GiantPanda", "Giraffe", "Gnat", "Gnu", "Goat", "Goldfinch", "Goosander", "Goose", "Gorilla", "Goshawk", "Grasshopper", "Grouse", "Guanaco", "GuineaFowl", "GuineaPig", "Gull", "Hamster", "Hare", "Hawk", "Hedgehog", "Heron", "Herring", "Hippo", "Hornet", "Horse", "Hummingbird", "Hyena", "Ibex", "Ibis", "Impala", "Jackal", "Jaguar", "Jay", "Jellyfish", "Kangaroo", "Kinkajou", "Kitten", "Kiwi", "Koala", "KomodoDragon", "Kouprey", "Kudu", "Ladybug", "Lapwing", "Lark", "Lemur", "Leopard", "Lion", "Lizard", "Llama", "Lobster", "Locust", "Loris", "Louse", "Lynx", "Lyrebird", "Magpie", "Mallard", "Mammoth", "Manatee", "Mandrill", "Marmoset", "Mink", "Mole", "Mongoose", "Monkey", "Moose", "Mosquito", "Moth", "Mouse", "Narwhal", "Newt", "Nightlingale", "Ocelot", "Octopus", "Okapi", "Opossum", "Ostrich", "Otter", "Owl", "Oyster", "Panda", "Panther", "Parrot", "Partridge", "Peafowl", "Pelican", "Penguin", "Pheasant", "Pig", "Pigeon", "Pika", "PolarBear", "Polecat", "Pony", "Porcupine", "Porpoise", "PrairieDog", "Pug", "Puma", "Puppy", "Quail", "Quelea", "Quetzal", "Rabbit", "Raccoon", "Ram", "Rat", "Raven", "RedDeer", "RedPanda", "Reindeer", "Rhinoceros", "Rook", "Rooster", "Salamander", "Salmon", "SandDollar", "Sandpiper", "Sardine", "SeaLion", "Seahorse", "Seal", "Shark", "Sheep", "Shrew", "Siamang", "Skunk", "Sloth", "Slug", "Snail", "Snake", "Snowshoe", "Sow", "Sparrow", "Spider", "Squid", "Squirrel", "Stalion", "Starling", "Stegosaurus", "Stoat", "Swan", "Tapir", "Tarsier", "Termite", "Tiger", "Toad", "Tortoise", "Turkey", "Turtle", "Vicuna", "Viper", "Vole", "Vulture", "Wallaby", "Walrus", "Wasp", "WaterBuffalo", "Weasel", "Whale", "Wolf", "Wolverine", "Wombat", "Woodpecker", "Worm", "Wren", "Yak", "Zebra", "Zebu" ];
