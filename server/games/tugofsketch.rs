use super::game;
use crate::api::{self, ChatMsg, GameMsg, Name, Stroke, VisibleEvent};
use arraydeque::{ArrayDeque, Wrapping};
use log::{debug, error, info, warn};
use quick_error::quick_error;
use rand::seq::IteratorRandom;
use slotmap::{new_key_type, SecondaryMap, SlotMap};
use std::{
    iter::repeat,
    time::{Duration, Instant},
};

type Slab<T> = SlotMap<Id, T>;

new_key_type! { pub struct Id; }
type Challenge = u16;

quick_error! {
    #[derive(Debug)]
    pub enum GameErr {
        NoOneLeft {
            description("No player remaining")
        }
        ImpossibleLeave {
            description("Attempted to kick absent player")
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
    /// Result screen for the previous round where `artist` was the artist
    RoundResults {
        artist: Id,
    },
    Playing {
        drawing: Vec<Stroke>,
        artist: Id,
        word: &'static str,
        lap: Instant,
        scores: SecondaryMap<Id, RoundScore>,
        anyone_guessed: bool,
    },
    EndResults,
}

/// Something that `Game` recieves back from the manager
#[derive(Debug)]
pub struct Feedback(Feedback_);

#[derive(Debug)]
enum Feedback_ {
    TickTimeout(u8),
    RevealLetter(u8),
    NextRound(u8),
    EndRound(u8),
    MakeMaster(Id),
    TooLate(Id, Challenge),
    Restart(u8),
}

pub struct Game {
    state:       Game_,
    players:     Slab<Player>,
    game_log:    ArrayDeque<[VisibleEvent; 20], Wrapping>,
    round_count: u8,
    round_no:    u8,
}

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

#[derive(Clone, Debug)]
struct Player {
    name:    Name,
    score:   Vec<RoundScore>,
    drop_id: Option<Challenge>,
}
impl Into<(Name, Vec<api::RoundScore>)> for Player {
    fn into(self) -> (Name, Vec<api::RoundScore>) {
        let Player { name, score, .. } = self;
        let score = score.into_iter().map(Into::into).collect();
        (name, score)
    }
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

const ROUND_LENGTH: i16 = 30;
const TICK_UPDATE: Duration = Duration::from_secs(10);
const REVEAL_INTERVAL: Duration = Duration::from_secs(10);
const TALLY_LENGTH: Duration = Duration::from_secs(5);
const DROP_DELAY: Duration = Duration::from_secs(4);
const JOIN_DELAY: Duration = Duration::from_secs(30);
const ROUND_COUNT: u8 = 3;
const RESTART_INTERVAL: Duration = Duration::from_secs(10);

impl Game {
    /// Returns a connected player, if there is indeed one remaining
    fn next_player(&self) -> Option<Id> {
        self.players
            .iter()
            .find(|p| p.1.drop_id.is_none())
            .map(|x| x.0)
    }

    fn api_scores(&self) -> api::Scoreboard {
        self.players.iter().map(|(_, x)| x.clone().into()).collect()
    }

    fn into_sync_msg(&self) -> GameMsg {
        use api::{
            GameMsg::HiddenEvent,
            GameScreen::{EndSummary, Lobby, Round, Scores},
        };
        let scores = self.api_scores();
        let history = self.game_log.iter().map(Clone::clone).collect();
        let screen = match self.state {
            Game_::Empty => {
                error!("Attempt to sync to empty state");
                panic!("tugofsketch:{} unreachable path", line!())
            }
            Game_::Lobby { leader } => Lobby {
                master: self.players[leader].name.clone(),
            },
            Game_::RoundResults { .. } => Scores,
            Game_::Playing {
                ref drawing,
                artist,
                lap,
                ..
            } => Round {
                timeout: ROUND_LENGTH - lap.elapsed().as_secs() as i16,
                drawing: drawing.clone(),
                artist:  self.players[artist].name.clone(),
            },
            Game_::EndResults => EndSummary,
        };
        HiddenEvent(api::HiddenEvent::Sync {
            screen,
            history,
            scores,
        })
    }

    /// Initiate a new round, with new artist, new word etc
    fn start_round(
        &mut self,
        instigator: Option<Id>,
    ) -> Option<(Broadcast, Cmd)> {
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
        macro_rules! start_round {
            ($next_artist:expr) => {{
                info!("Starting round {}", self.round_no);
                debug!("scores: {:#?}", self.players);
                let artist = self.players[$next_artist].name.clone();
                let word = WORD_LIST[rand::random::<u8>() as usize];
                let mut scores =
                    SecondaryMap::with_capacity(self.players.capacity());
                scores.insert($next_artist, RoundScore::Artist(0));
                self.state = Game_::Playing {
                    drawing: vec![],
                    artist: $next_artist,
                    word,
                    lap: Instant::now(),
                    anyone_guessed: false,
                    scores,
                };
                let make_msg = |word| {
                    HiddenEvent(Start(RoundStart {
                        timeout: ROUND_LENGTH,
                        artist: artist.clone(),
                        word,
                    }))
                };
                self.game_log.push_front(SyncStart(artist.clone()));
                let msg = make_msg(Guess(word.chars().count()));
                let artist_msg = make_msg(Artist(word.to_string()));

                let rc = self.round_no;
                let revealback = Feedback(RevealLetter(rc));
                let tickback = Feedback(TickTimeout(rc));
                let overback = Feedback(EndRound(rc));
                Some((
                    broadcast!(to_all_but, $next_artist, msg, Some(artist_msg)),
                    game::Cmd::InMultiple(vec![
                        (TICK_UPDATE, tickback),
                        (REVEAL_INTERVAL, revealback),
                        (Duration::from_secs(ROUND_LENGTH as u64), overback),
                    ]),
                ))
            }};
        }
        match self.state {
            Game_::Lobby { leader } if instigator == Some(leader) => {
                self.round_no = 1;
                start_round!(leader)
            }
            Game_::RoundResults { artist } => {
                let next_artist = self
                    .players
                    .iter()
                    .find(|p| p.1.drop_id.is_none() && p.0 > artist)
                    .or_else(|| self.players.iter().next())
                    .map(|x| x.0)
                    .unwrap_or_else(|| {
                        error!("Couldn't find an artist for the next round");
                        artist
                    });
                self.round_no += 1;
                start_round!(next_artist)
            }
            Game_::EndResults => {
                self.players
                    .iter_mut()
                    .for_each(|(_, player)| player.score.clear());
                self.round_no = 1;
                let next_artist = self.next_player()?;
                start_round!(next_artist)
            }
            _ => {
                error!("Attempt to start new round in an incompatible state");
                None
            }
        }
    }

    /// check if player guessed the word correctly. Update game state
    /// accordingly. If the guess was correct, returns `Some(_)`, otherwise
    /// `None`. If all players have guessed, returns `Some(true)`.
    fn guesses(&mut self, player: Id, message: &[u8]) -> Option<bool> {
        use self::{
            api::VisibleEvent,
            RoundScore::{Artist, Guessed},
        };
        if let Game_::Playing {
            artist,
            ref mut scores,
            ref mut anyone_guessed,
            word,
            ..
        } = self.state
        {
            if !scores.contains_key(player) && word.as_bytes() == message {
                let score = if *anyone_guessed {
                    1
                } else {
                    *anyone_guessed = true;
                    3
                };
                let guesser = self.players[player].name.clone();
                self.game_log.push_front(VisibleEvent::Guessed(guesser));
                scores.insert(player, Guessed(score));
                // NOTE: problem here? aliasing safe though (`player != artist`
                // because `scores.insert(artist,..)` at `start_round`)
                if let Artist(ref mut artist_score) = scores[artist] {
                    *artist_score += score;
                } else {
                    error!("The artist score was not properly set");
                    scores.insert(artist, Artist(score));
                }
                // Did all player guess?
                Some(self.players.len() == scores.len())
            } else {
                None
            }
        } else {
            warn!("A guess occured outside of a regular play time");
            Some(true) // Force termination of round if there is something fishy
        }
    }

    /// Set game state to "Round summary" and indicate that all players who
    /// haven't guessed yet have failed.
    /// Returns the scores for the round that is being terminated.
    fn end_round(&mut self, word: String) -> Result<(Broadcast, Cmd), GameErr> {
        use self::RoundScore::Failed;
        use api::{
            GameMsg::HiddenEvent, HiddenEvent::Over, VisibleEvent::SyncOver,
        };
        info!("Ending round {}", self.round_no);
        if let Game_::Playing {
            artist,
            ref mut scores,
            ..
        } = self.state
        {
            let round_scores = self
                .players
                .iter_mut()
                .map(|(id, player)| {
                    let score = scores.remove(id).unwrap_or(Failed);
                    player.score.push(score.clone());
                    (player.name.clone(), score.into())
                })
                .collect();
            self.state = Game_::RoundResults { artist };
            self.game_log.push_front(SyncOver(word.clone()));
            let send = Over(word.to_string(), round_scores);
            let msg = Feedback_::NextRound(self.round_no);
            let cmd = game::Cmd::In(TALLY_LENGTH, Feedback(msg));
            Ok((broadcast!(to_all, HiddenEvent(send)), cmd))
        } else {
            error!("Ending a round that is not happening");
            Err(GameErr::BadEnd)
        }
    }

    fn leaves(&mut self, player: Id) -> Result<(Broadcast, Cmd), GameErr> {
        use api::VisibleEvent::Left;
        let immediately = |msg| game::Cmd::Immediately(Feedback(msg));
        match self.players.remove(player) {
            Some(Player { name, .. }) => {
                let left = GameMsg::VisibleEvent(Left(name.clone()));
                self.game_log.push_front(Left(name));
                if let Some(id) = self.next_player() {
                    if self.players.len() == 1 {
                        self.round_no = 0;
                        self.players[id].score = Vec::new();
                        self.state = Game_::Lobby { leader: id };
                    };
                    let command = match self.state {
                        Game_::Lobby { ref leader } if *leader == player => {
                            immediately(Feedback_::MakeMaster(id))
                        }
                        Game_::Playing {
                            artist,
                            ref mut scores,
                            ..
                        } => {
                            scores.remove(player);
                            if artist == player {
                                let rc = self.round_no;
                                immediately(Feedback_::EndRound(rc))
                            } else {
                                game::Cmd::None
                            }
                        }
                        _ => game::Cmd::None,
                    };
                    Ok((broadcast!(to_all, left), command))
                } else {
                    Err(GameErr::NoOneLeft)
                }
            }
            None => Err(GameErr::ImpossibleLeave),
        }
    }

    fn try_terminate(
        &mut self,
        id: Id,
        challenge: Challenge,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        if Some(challenge) == self.players.get(id).and_then(|x| x.drop_id) {
            info!("{:?} was too late, removing them...", id);
            self.leaves(id)
        } else {
            Ok((broadcast!(nothing), game::Cmd::None))
        }
    }

    fn respond(
        &mut self,
        player: Id,
        request: api::GameReq,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        use self::api::GameReq;
        let msg = match request {
            GameReq::Sync => None,
            GameReq::Start => self.start_round(Some(player)),
            GameReq::Canvas(msg) => match self.state {
                Game_::Playing {
                    ref mut drawing,
                    artist,
                    ..
                } => {
                    if artist != player {
                        None
                    } else {
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
                        Some((
                            broadcast!(to_all_but, artist, msg, None),
                            game::Cmd::None,
                        ))
                    }
                }
                _ => None,
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
                        let guesser = self.players[player].name.clone();
                        let msg = VisibleEvent(Guessed(guesser));
                        let correct = HiddenEvent(Correct(word.to_string()));
                        let cmd = if complete {
                            let end_round = Feedback(EndRound(self.round_no));
                            Cmd::Immediately(end_round)
                        } else {
                            Cmd::None
                        };
                        Some((
                            broadcast!(to_all_but, player, msg, Some(correct)),
                            cmd,
                        ))
                    } else {
                        let final_msg = api::VisibleEvent::Message(ChatMsg {
                            content: msg,
                            author:  self.players[player].name.clone(),
                        });
                        self.game_log.push_front(final_msg.clone());
                        Some((
                            broadcast!(to_all, VisibleEvent(final_msg)),
                            Cmd::None,
                        ))
                    }
                }
                _ => {
                    let final_msg = api::VisibleEvent::Message(ChatMsg {
                        content: msg,
                        author:  self.players[player].name.clone(),
                    });
                    self.game_log.push_front(final_msg.clone());
                    Some((
                        broadcast!(to_all, GameMsg::VisibleEvent(final_msg)),
                        game::Cmd::None,
                    ))
                }
            },
        };
        let msg = msg.unwrap_or_else(|| {
            (
                broadcast!(to_unique, player, self.into_sync_msg()),
                game::Cmd::None,
            )
        });
        Ok(msg)
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
        let cr = self.round_no;
        match (&mut self.state, msg) {
            (_, TooLate(id, challenge)) => self.try_terminate(id, challenge),
            (Lobby { ref mut leader }, MakeMaster(id)) => {
                *leader = id;
                let msg = HiddenEvent(Mastery);
                Ok((broadcast!(to_unique, id, msg), game::Cmd::None))
            }
            (Playing { word, artist, .. }, RevealLetter(r)) if r == cr => {
                let mut rng = rand::thread_rng();
                let (index, letter) =
                    word.chars().enumerate().choose(&mut rng).unwrap();
                let msg = HiddenEvent(Reveal(index, letter));
                let cmd = Feedback(RevealLetter(self.round_no));
                Ok((
                    broadcast!(to_all_but, *artist, msg, None),
                    game::Cmd::In(REVEAL_INTERVAL, cmd),
                ))
            }
            (Playing { word, .. }, EndRound(r)) if r == cr => {
                let word = word.to_string();
                self.end_round(word)
            }
            (Playing { lap, word, .. }, TickTimeout(r)) if r == cr => {
                let timeout = ROUND_LENGTH - lap.elapsed().as_secs() as i16;
                if timeout <= 0 {
                    let word = word.to_string();
                    self.end_round(word)
                } else {
                    let send = TimeoutSync(timeout);
                    let msg = TickTimeout(self.round_no);
                    let cmd = game::Cmd::In(TICK_UPDATE, Feedback(msg));
                    Ok((broadcast!(to_all, HiddenEvent(send)), cmd))
                }
            }
            (RoundResults { .. }, NextRound(r)) if r == cr => {
                if cr >= self.round_count {
                    self.game_over()
                } else {
                    self.start_round(None).ok_or(GameErr::BadStart)
                }
            }
            (EndResults, Restart(r)) if r == cr => {
                self.start_round(None).ok_or(GameErr::BadStart)
            }
            (RoundResults { .. }, TickTimeout(_))
            | (Playing { .. }, TickTimeout(_))
            | (RoundResults { .. }, EndRound(_))
            | (Playing { .. }, EndRound(_))
            | (RoundResults { .. }, RevealLetter(_))
            | (Playing { .. }, RevealLetter(_)) => {
                Ok((broadcast!(nothing), game::Cmd::None))
            }
            (_state, feedback) => {
                warn!("invalid feedback: {:?} for game state", feedback);
                Ok((broadcast!(nothing), game::Cmd::None))
            }
        }
    }

    fn game_over(&mut self) -> Result<(Broadcast, Cmd), GameErr> {
        use self::Feedback_::Restart;
        let mut scores = self.api_scores();
        scores.sort_unstable_by_key(|(_, s)| api::score_total(s));

        self.game_log.push_front(api::VisibleEvent::SyncComplete);
        self.state = Game_::EndResults;

        let msg = GameMsg::HiddenEvent(api::HiddenEvent::Complete(scores));
        Ok((
            broadcast!(to_all, msg),
            game::Cmd::In(RESTART_INTERVAL, Feedback(Restart(self.round_no))),
        ))
    }
}
impl game::Game<Id> for Game {
    type Error = GameErr;
    type Feedback = Feedback;
    type Request = api::GameReq;
    type Response = GameMsg;

    fn new() -> Self {
        Game {
            state:       Game_::Empty,
            players:     Slab::with_key(),
            game_log:    ArrayDeque::new(),
            round_count: ROUND_COUNT,
            round_no:    0,
        }
    }

    fn expect(
        &mut self,
        name: Name,
    ) -> Result<(Option<Id>, Broadcast, Cmd), GameErr> {
        use self::{Feedback_::TooLate, Game_::*, RoundScore::Absent};
        use api::VisibleEvent::Joined;
        if self
            .players
            .values()
            .any(|Player { name: n, .. }| n == &name)
        {
            Ok((None, broadcast!(nothing), game::Cmd::None))
        } else {
            let rounds_elapsed = match self.state {
                RoundResults { .. } | Lobby { .. } | EndResults => {
                    self.round_no as usize
                }
                Playing { .. } | Empty => {
                    self.round_no.saturating_sub(1) as usize
                }
            };
            let drop_id = rand::random();
            let id = self.players.insert(Player {
                name:    name.clone(),
                score:   repeat(Absent).take(rounds_elapsed).collect(),
                drop_id: Some(drop_id),
            });
            let cmd = game::Cmd::In(JOIN_DELAY, Feedback(TooLate(id, drop_id)));
            match self.state {
                Empty => {
                    self.state = Lobby { leader: id };
                }
                Playing { ref mut scores, .. } => {
                    scores.insert(id, Absent);
                }
                Lobby { .. } | RoundResults { .. } | EndResults => {}
            };
            self.game_log.push_front(Joined(name.clone()));
            let msg = GameMsg::VisibleEvent(Joined(name));
            Ok((Some(id), broadcast!(to_all, msg), cmd))
        }
    }

    fn joins(&mut self, player: Id) -> Result<(bool, Broadcast, Cmd), GameErr> {
        if self
            .players
            .get_mut(player)
            .map(|x| x.drop_id = None)
            .is_some()
        {
            Ok((true, broadcast!(nothing), game::Cmd::None))
        } else {
            Ok((false, broadcast!(nothing), game::Cmd::None))
        }
    }

    fn tells(
        &mut self,
        request: game::Request<Id, api::GameReq, Feedback>,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        match request {
            game::Request::Leaves(id) => {
                let drop_id = rand::random();
                self.players[id].drop_id = Some(drop_id);
                let cmd = game::Cmd::In(
                    DROP_DELAY,
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
