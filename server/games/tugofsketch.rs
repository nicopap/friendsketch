use super::game::{self, ExpectResponse};
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
}

/// Something that `Game` recieves back from the manager
#[derive(Debug)]
pub struct Feedback {
    /// The message itself
    msg: Feedback_,
}

#[derive(Debug)]
enum Feedback_ {
    TickTimeout(u8),
    RevealLetter(u8),
    NextRound(u8),
    EndRound(u8),
    MakeMaster(Id),
    TooLate(Id, Challenge),
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
const TALLY_LENGTH: Duration = Duration::from_secs(6);
const DROP_DELAY: Duration = Duration::from_secs(4);

impl Game {
    fn api_scores(&self) -> api::Scoreboard {
        self.players.iter().map(|(_, x)| x.clone().into()).collect()
    }

    fn into_sync_msg(&self) -> GameMsg {
        use api::{
            GameMsg::HiddenEvent,
            GameScreen::{Lobby, Round, Scores},
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
                let feedback = |msg| Feedback { msg };
                let revealback = feedback(RevealLetter(rc));
                let tickback = feedback(TickTimeout(rc));
                let overback = feedback(EndRound(rc));
                Some((
                    broadcast!(to_all_but, $next_artist, msg, Some(artist_msg)),
                    game::Cmd::In(vec![
                        (TICK_UPDATE, tickback),
                        (REVEAL_INTERVAL, revealback),
                        (Duration::from_secs(ROUND_LENGTH as u64), overback),
                    ]),
                ))
            }};
        }
        match self.state {
            Game_::Lobby { leader } if instigator == Some(leader) => {
                self.round_no = 0;
                start_round!(leader)
            }
            Game_::RoundResults { artist } => {
                let next_artist: Id = self
                    .players
                    .iter()
                    .map(|tuple| tuple.0)
                    .skip_while(|id| *id != artist)
                    .nth(1)
                    .or_else(|| self.players.iter().map(|t| t.0).nth(0))
                    .unwrap_or_else(|| {
                        error!("Couldn't find an artist for the next round");
                        artist
                    });
                self.round_no += 1;
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
    fn end_round(&mut self) -> Vec<(Name, api::RoundScore)> {
        use self::RoundScore::Failed;
        info!("Ending round {}", self.round_no);
        if let Game_::Playing {
            artist,
            ref mut scores,
            ..
        } = self.state
        {
            let ret = self
                .players
                .iter_mut()
                .map(|(id, player)| {
                    let score = scores.remove(id).unwrap_or(Failed);
                    player.score.push(score.clone());
                    (player.name.clone(), score.into())
                })
                .collect();
            self.state = Game_::RoundResults { artist };
            ret
        } else {
            error!("Ending a round that is not happening");
            Vec::new()
        }
    }

    fn leaves(&mut self, player: Id) -> Result<(Broadcast, Cmd), GameErr> {
        use api::VisibleEvent::Left;
        let immediately = |msg| game::Cmd::Immediately(Feedback { msg });
        match self.players.remove(player) {
            Some(Player { name, .. }) => {
                let left = GameMsg::VisibleEvent(Left(name.clone()));
                self.game_log.push_front(Left(name));
                if let Some((id, _)) = self.players.iter().next() {
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

    fn try_drop(
        &mut self,
        id: Id,
        challenge: Challenge,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        if Some(challenge) == self.players.get(id).and_then(|x| x.drop_id) {
            self.leaves(id)
        } else {
            info!("challenged {:?} to no avail", id);
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
                    use self::game::Cmd;
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
                            let end_round = Feedback {
                                msg: Feedback_::EndRound(self.round_no),
                            };
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
        Feedback { msg }: Feedback,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        use self::{
            api::{
                GameMsg::HiddenEvent,
                HiddenEvent::{Mastery, Over, Reveal, TimeoutSync},
                VisibleEvent::SyncOver,
            },
            Feedback_::*,
            Game_::*,
        };
        let cr = self.round_no;
        match (&mut self.state, msg) {
            (_, TooLate(id, challenge)) => self.try_drop(id, challenge),
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
                let cmd = Feedback {
                    msg: Feedback_::RevealLetter(self.round_no),
                };
                Ok((
                    broadcast!(to_all_but, *artist, msg, None),
                    game::Cmd::In(vec![(REVEAL_INTERVAL, cmd)]),
                ))
            }
            (Playing { word, .. }, EndRound(r)) if r == cr => {
                let word = word.to_string();
                let scores = self.end_round();
                self.game_log.push_front(SyncOver(word.clone()));
                let send = Over(word, scores);
                let msg = Feedback_::NextRound(self.round_no);
                let cmd = game::Cmd::In(vec![(TALLY_LENGTH, Feedback { msg })]);
                Ok((broadcast!(to_all, HiddenEvent(send)), cmd))
            }
            (Playing { lap, word, .. }, TickTimeout(r)) if r == cr => {
                let timeout = ROUND_LENGTH - lap.elapsed().as_secs() as i16;
                let (classic_reply, cmd) = if timeout <= 0 {
                    self.game_log.push_front(SyncOver(word.to_string()));
                    let word_s = word.to_string();
                    let scores = self.end_round();
                    let send = Over(word_s, scores);
                    let msg = Feedback_::NextRound(self.round_no);
                    let cmd =
                        game::Cmd::In(vec![(TALLY_LENGTH, Feedback { msg })]);
                    (HiddenEvent(send), cmd)
                } else {
                    let send = TimeoutSync(timeout);
                    let msg = Feedback_::TickTimeout(self.round_no);
                    let cmd =
                        game::Cmd::In(vec![(TICK_UPDATE, Feedback { msg })]);
                    (HiddenEvent(send), cmd)
                };
                Ok((broadcast!(to_all, classic_reply), cmd))
            }
            (RoundResults { .. }, TickTimeout(_))
            | (RoundResults { .. }, RevealLetter(_)) => {
                Ok((broadcast!(nothing), game::Cmd::None))
            }
            (RoundResults { .. }, NextRound(r)) if r == cr => {
                Ok(self.start_round(None).unwrap())
            }
            (_state, feedback) => {
                warn!("invalid feedback: {:?} for game state", feedback);
                Ok((broadcast!(nothing), game::Cmd::None))
            }
        }
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
            round_count: 5,
            round_no:    0,
        }
    }

    fn expect(&mut self, name: Name) -> ExpectResponse<Id, Feedback> {
        use self::{Game_::*, RoundScore::Absent};
        if self
            .players
            .values()
            .any(|Player { name: n, .. }| n == &name)
        {
            ExpectResponse::Refuse
        } else {
            let rounds_elapsed = match self.state {
                RoundResults { .. } => self.round_no as usize + 1,
                Playing { .. } | Empty | Lobby { .. } => self.round_no as usize,
            };
            let score = repeat(Absent).take(rounds_elapsed).collect();
            let drop_id_ = rand::random();
            let drop_id = Some(drop_id_);
            let id = self.players.insert(Player {
                name,
                score,
                drop_id,
            });
            let cmd = game::Cmd::In(vec![(
                DROP_DELAY,
                Feedback {
                    msg: Feedback_::TooLate(id, drop_id_),
                },
            )]);
            match self.state {
                Empty => {
                    self.state = Lobby { leader: id };
                }
                Playing { ref mut scores, .. } => {
                    scores.insert(id, Absent);
                }
                Lobby { .. } | RoundResults { .. } => {}
            };
            ExpectResponse::Accept(id, cmd)
        }
    }

    fn joins(&mut self, player: Id) -> Result<(bool, Broadcast, Cmd), GameErr> {
        use api::VisibleEvent::Joined;
        if let Some(name) = self.players.get_mut(player).map(|x| {
            x.drop_id = None;
            x.name.clone()
        }) {
            self.game_log.push_front(Joined(name.clone()));
            let msg = GameMsg::VisibleEvent(Joined(name));
            Ok((true, broadcast!(to_all, msg), game::Cmd::None))
        } else {
            Ok((false, broadcast!(nothing), game::Cmd::None))
        }
    }

    fn tells(
        &mut self,
        request: game::Request<Id, api::GameReq, Feedback>,
    ) -> Result<(Broadcast, Cmd), GameErr> {
        match request {
            game::Request::Leaves(id) => self.leaves(id),
            game::Request::Feedback(feedback) => self.feedback(feedback),
            game::Request::Message(id, msg) => self.respond(id, msg),
        }
    }
}

#[rustfmt::skip]
static WORD_LIST: [&str; 256] = [ "Aardvark", "Albatross", "Alligator", "Alpaca", "Ant", "Anteater", "Antelope", "Ape", "Armadillo", "Baboon", "Badger", "Barracuda", "Bat", "Bear", "Beaver", "Bee", "Beetle", "Bird", "Bison", "Boar", "Bobcat", "Buffalo", "Bull", "Butterfly", "Camel", "Caribou", "Cassowary", "Cat", "Caterpillar", "Cattle", "Chameleon", "Chamois", "Cheetah", "Chicken", "Chimpanzee", "Chinchilla", "Chipmunk", "Chough", "Civet", "Coati", "Cobra", "Cockroach", "Cod", "Cormorant", "Cougar", "Cow", "Coyote", "Crab", "Crane", "Crocodile", "Crow", "Cuckoo", "Curlew", "Deer", "Dinosaur", "Doe", "Dog", "Dogfish", "Dolphin", "Donkey", "Dotterel", "Dove", "Dragonfly", "Dromedary", "Duck", "Dugong", "Dunlin", "Eagle", "Echidna", "Eel", "Eland", "Elephant", "ElephantSeal", "Elk", "Emu", "Falcon", "Ferret", "Finch", "Fish", "Flamingo", "Fly", "Fox", "Frog", "Gaur", "Gazelle", "Gerbil", "GiantPanda", "Giraffe", "Gnat", "Gnu", "Goat", "Goldfinch", "Goosander", "Goose", "Gorilla", "Goshawk", "Grasshopper", "Grouse", "Guanaco", "GuineaFowl", "GuineaPig", "Gull", "Hamster", "Hare", "Hawk", "Hedgehog", "Heron", "Herring", "Hippo", "Hornet", "Horse", "Hummingbird", "Hyena", "Ibex", "Ibis", "Impala", "Jackal", "Jaguar", "Jay", "Jellyfish", "Kangaroo", "Kinkajou", "Kitten", "Kiwi", "Koala", "KomodoDragon", "Kouprey", "Kudu", "Ladybug", "Lapwing", "Lark", "Lemur", "Leopard", "Lion", "Lizard", "Llama", "Lobster", "Locust", "Loris", "Louse", "Lynx", "Lyrebird", "Magpie", "Mallard", "Mammoth", "Manatee", "Mandrill", "Marmoset", "Mink", "Mole", "Mongoose", "Monkey", "Moose", "Mosquito", "Moth", "Mouse", "Narwhal", "Newt", "Nightlingale", "Ocelot", "Octopus", "Okapi", "Opossum", "Ostrich", "Otter", "Owl", "Oyster", "Panda", "Panther", "Parrot", "Partridge", "Peafowl", "Pelican", "Penguin", "Pheasant", "Pig", "Pigeon", "Pika", "PolarBear", "Polecat", "Pony", "Porcupine", "Porpoise", "PrairieDog", "Pug", "Puma", "Puppy", "Quail", "Quelea", "Quetzal", "Rabbit", "Raccoon", "Ram", "Rat", "Raven", "RedDeer", "RedPanda", "Reindeer", "Rhinoceros", "Rook", "Rooster", "Salamander", "Salmon", "SandDollar", "Sandpiper", "Sardine", "SeaLion", "Seahorse", "Seal", "Shark", "Sheep", "Shrew", "Siamang", "Skunk", "Sloth", "Slug", "Snail", "Snake", "Snowshoe", "Sow", "Sparrow", "Spider", "Squid", "Squirrel", "Stalion", "Starling", "Stegosaurus", "Stoat", "Swan", "Tapir", "Tarsier", "Termite", "Tiger", "Toad", "Tortoise", "Turkey", "Turtle", "Vicuna", "Viper", "Vole", "Vulture", "Wallaby", "Walrus", "Wasp", "WaterBuffalo", "Weasel", "Whale", "Wolf", "Wolverine", "Wombat", "Woodpecker", "Worm", "Wren", "Yak", "Zebra", "Zebu" ];
