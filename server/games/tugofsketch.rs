use super::game::{self, JoinResponse, LeaveResponse};
use crate::api::{self, ChatMsg, Name, Stroke, VisibleEvent};
use arraydeque::{ArrayDeque, Wrapping};
use log::{debug, error, info, warn};
use rand::seq::IteratorRandom;
use slotmap::{new_key_type, SecondaryMap, SlotMap};
use std::{
    iter::repeat,
    time::{Duration, Instant},
};

type Slab<T> = SlotMap<Id, T>;

new_key_type! {
    pub struct Id;
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
pub struct Feedback {
    /// The message itself
    msg: Feedback_,
    /// The round number in which this `Feedback` was sent. So as to make sure
    /// that we do not concider a message sent during a previous round.
    sent_round: u8,
}
#[derive(Debug)]
enum Feedback_ {
    TickTimeout,
    RevealLetter,
    NextRound,
    EndRound,
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
    name:  Name,
    score: Vec<RoundScore>,
}
impl Into<(Name, Vec<api::RoundScore>)> for Player {
    fn into(self) -> (Name, Vec<api::RoundScore>) {
        let Player { name, score, .. } = self;
        let score = score.into_iter().map(Into::into).collect();
        (name, score)
    }
}

type Broadcast = game::Broadcast<Id, api::GameMsg>;
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

const TALLY_LENGTH: Duration = Duration::from_secs(6);

impl Game {
    fn api_scores(&self) -> api::Scoreboard {
        self.players.iter().map(|(_, x)| x.clone().into()).collect()
    }

    fn into_sync_msg(&self) -> api::GameMsg {
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
        use self::api::{
            GameMsg::HiddenEvent,
            Guess::{Artist, Guess},
            HiddenEvent::Start,
            RoundStart,
            VisibleEvent::SyncStart,
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
                let delay = TICK_UPDATE;
                let feedback = Feedback {
                    sent_round: self.round_no,
                    msg:        Feedback_::TickTimeout,
                };
                Some((
                    broadcast!(to_all_but, $next_artist, msg, Some(artist_msg)),
                    game::Cmd::In(delay, feedback),
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

    /// Update players scores accordingly. Returns whehter all players guessed
    /// correctly
    fn guesses_correctly(&mut self, player: Id) -> bool {
        use self::RoundScore::{Artist, Guessed};
        if let Game_::Playing {
            artist,
            ref mut scores,
            ref mut anyone_guessed,
            ..
        } = self.state
        {
            if !scores.contains_key(player) {
                let score = if *anyone_guessed {
                    1
                } else {
                    *anyone_guessed = true;
                    3
                };
                scores.insert(player, Guessed(score));
                // NOTE: problem here? aliasing safe though (`player != artist`
                // because `scores.insert(artist,..)` at `start_round`)
                if let Artist(ref mut artist_score) = scores[artist] {
                    *artist_score += score;
                } else {
                    error!("The artist score was not set");
                    scores.insert(artist, Artist(score));
                }
            };
            self.players.len() == scores.len() // Did all player guess?
        } else {
            warn!("A guess occured outside of a regular play time");
            true // Force termination of round if something fishy is going on
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
}
impl game::Game<Id> for Game {
    type Error = ();
    type Feedback = Feedback;
    type Request = api::GameReq;
    type Response = api::GameMsg;

    fn new() -> Self {
        Game {
            state:       Game_::Empty,
            players:     Slab::with_key(),
            game_log:    ArrayDeque::new(),
            round_count: 5,
            round_no:    0,
        }
    }

    fn joins(&mut self, name: Name) -> JoinResponse<Id> {
        if self
            .players
            .values()
            .any(|Player { name: n, .. }| n == &name)
        {
            JoinResponse::Refuse
        } else {
            use self::{api::VisibleEvent, Game_::*, RoundScore::Absent};
            self.game_log.push_front(VisibleEvent::Joined(name.clone()));
            let rounds_elapsed = match self.state {
                RoundResults { .. } => self.round_no as usize + 1,
                Playing { .. } | Empty | Lobby { .. } => self.round_no as usize,
            };
            let score = repeat(Absent).take(rounds_elapsed).collect();
            let id = self.players.insert(Player { name, score });
            match self.state {
                Empty => {
                    self.state = Lobby { leader: id };
                }
                Playing { ref mut scores, .. } => {
                    scores.insert(id, Absent);
                }
                Lobby { .. } | RoundResults { .. } => {}
            };
            JoinResponse::Accept(id)
        }
    }

    fn leaves(&mut self, player: Id) -> LeaveResponse<Id, api::GameMsg, ()> {
        use self::api::{
            GameMsg::HiddenEvent,
            HiddenEvent::{Mastery, Over},
            VisibleEvent::{Left, SyncOver},
        };
        match self.players.remove(player) {
            Some(Player { name, .. }) => {
                self.game_log.push_front(Left(name.clone()));
                if let Some((id, _)) = self.players.iter().next() {
                    let response = match self.state {
                        Game_::Lobby { ref mut leader }
                            if *leader == player =>
                        {
                            *leader = id;
                            broadcast!(to_unique, id, HiddenEvent(Mastery))
                        }
                        Game_::Playing {
                            ref mut artist,
                            word,
                            ref mut scores,
                            ..
                        } => {
                            scores.remove(player);
                            if *artist == player {
                                *artist = id;
                                let scores = self.end_round();
                                let msg =
                                    HiddenEvent(Over(word.to_string(), scores));
                                self.game_log
                                    .push_back(SyncOver(word.to_string()));
                                broadcast!(to_all, msg)
                            } else {
                                broadcast!(nothing)
                            }
                        }
                        _ => broadcast!(nothing),
                    };
                    if self.players.len() == 1 {
                        self.round_no = 0;
                        self.players[id].score = Vec::new();
                        self.state = Game_::Lobby { leader: id };
                    }
                    LeaveResponse::Successfully(name, response)
                } else {
                    LeaveResponse::Empty(name)
                }
            }
            None => LeaveResponse::Failed(()),
        }
    }

    fn tells(
        &mut self,
        player: Id,
        request: api::GameReq,
    ) -> Result<(Broadcast, Cmd), ()> {
        use self::api::{GameMsg, GameReq};
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
            GameReq::Chat(content) => match self.state {
                Game_::Playing { artist, word, .. }
                    if player != artist
                        && word.as_bytes() == content.as_bytes() =>
                {
                    use api::{
                        GameMsg::HiddenEvent, HiddenEvent::Correct,
                        VisibleEvent::Guessed,
                    };
                    let all_guessed = self.guesses_correctly(player);
                    let guesser = self.players[player].name.clone();
                    let msg = GameMsg::VisibleEvent(Guessed(guesser));
                    let correct = Some(HiddenEvent(Correct(word.to_string())));
                    let cmd = if all_guessed {
                        let end_round = Feedback {
                            sent_round: self.round_no,
                            msg:        Feedback_::EndRound,
                        };
                        game::Cmd::Immediately(end_round)
                    } else {
                        game::Cmd::None
                    };
                    Some((broadcast!(to_all_but, player, msg, correct), cmd))
                }
                _ => {
                    let final_msg = api::VisibleEvent::Message(ChatMsg {
                        content,
                        author: self.players[player].name.clone(),
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
        Feedback { msg, sent_round }: Feedback,
    ) -> Result<(Broadcast, Cmd), ()> {
        use self::{
            api::{
                GameMsg::HiddenEvent,
                HiddenEvent::{Over, Reveal, TimeoutSync},
                VisibleEvent::SyncOver,
            },
            Feedback_::*,
            Game_::*,
        };
        if sent_round != self.round_no {
            warn!("receive feedback from previous round");
            return Ok((broadcast!(nothing), game::Cmd::None));
        };
        match (&self.state, msg) {
            (Playing { word, artist, .. }, RevealLetter) => {
                let mut rng = rand::thread_rng();
                let (index, letter) =
                    word.chars().enumerate().choose(&mut rng).unwrap();
                let msg = HiddenEvent(Reveal(index, letter));
                Ok((
                    broadcast!(to_all_but, *artist, msg, None),
                    game::Cmd::None,
                ))
            }
            (Playing { word, .. }, EndRound) => {
                let word = word.to_string();
                let scores = self.end_round();
                self.game_log.push_front(SyncOver(word.clone()));
                let send = Over(word, scores);
                let msg = Feedback_::NextRound;
                let cmd =
                    game::Cmd::In(TALLY_LENGTH, Feedback { sent_round, msg });
                Ok((broadcast!(to_all, HiddenEvent(send)), cmd))
            }
            (Playing { lap, word, .. }, TickTimeout) => {
                let timeout = ROUND_LENGTH - lap.elapsed().as_secs() as i16;
                let (classic_reply, cmd) = if timeout <= 0 {
                    self.game_log.push_front(SyncOver(word.to_string()));
                    let word_s = word.to_string();
                    let scores = self.end_round();
                    let send = Over(word_s, scores);
                    let msg = Feedback_::NextRound;
                    let cmd = game::Cmd::In(
                        TALLY_LENGTH,
                        Feedback { sent_round, msg },
                    );
                    (HiddenEvent(send), cmd)
                } else {
                    let send = TimeoutSync(timeout);
                    let msg = Feedback_::TickTimeout;
                    let cmd = game::Cmd::In(
                        TICK_UPDATE,
                        Feedback { sent_round, msg },
                    );
                    (HiddenEvent(send), cmd)
                };
                Ok((broadcast!(to_all, classic_reply), cmd))
            }
            (RoundResults { .. }, NextRound) => {
                Ok(self.start_round(None).unwrap())
            }
            (_state, feedback) => {
                warn!("invalid feedback: {:?} for game state", feedback);
                Ok((broadcast!(nothing), game::Cmd::None))
            }
        }
    }
}

#[rustfmt::skip]
static WORD_LIST: [&str; 256] = [ "Aardvark", "Albatross", "Alligator", "Alpaca", "Ant", "Anteater", "Antelope", "Ape", "Armadillo", "Baboon", "Badger", "Barracuda", "Bat", "Bear", "Beaver", "Bee", "Beetle", "Bird", "Bison", "Boar", "Bobcat", "Buffalo", "Bull", "Butterfly", "Camel", "Caribou", "Cassowary", "Cat", "Caterpillar", "Cattle", "Chameleon", "Chamois", "Cheetah", "Chicken", "Chimpanzee", "Chinchilla", "Chipmunk", "Chough", "Civet", "Coati", "Cobra", "Cockroach", "Cod", "Cormorant", "Cougar", "Cow", "Coyote", "Crab", "Crane", "Crocodile", "Crow", "Cuckoo", "Curlew", "Deer", "Dinosaur", "Doe", "Dog", "Dogfish", "Dolphin", "Donkey", "Dotterel", "Dove", "Dragonfly", "Dromedary", "Duck", "Dugong", "Dunlin", "Eagle", "Echidna", "Eel", "Eland", "Elephant", "ElephantSeal", "Elk", "Emu", "Falcon", "Ferret", "Finch", "Fish", "Flamingo", "Fly", "Fox", "Frog", "Gaur", "Gazelle", "Gerbil", "GiantPanda", "Giraffe", "Gnat", "Gnu", "Goat", "Goldfinch", "Goosander", "Goose", "Gorilla", "Goshawk", "Grasshopper", "Grouse", "Guanaco", "GuineaFowl", "GuineaPig", "Gull", "Hamster", "Hare", "Hawk", "Hedgehog", "Heron", "Herring", "Hippo", "Hornet", "Horse", "Hummingbird", "Hyena", "Ibex", "Ibis", "Impala", "Jackal", "Jaguar", "Jay", "Jellyfish", "Kangaroo", "Kinkajou", "Kitten", "Kiwi", "Koala", "KomodoDragon", "Kouprey", "Kudu", "Ladybug", "Lapwing", "Lark", "Lemur", "Leopard", "Lion", "Lizard", "Llama", "Lobster", "Locust", "Loris", "Louse", "Lynx", "Lyrebird", "Magpie", "Mallard", "Mammoth", "Manatee", "Mandrill", "Marmoset", "Mink", "Mole", "Mongoose", "Monkey", "Moose", "Mosquito", "Moth", "Mouse", "Narwhal", "Newt", "Nightlingale", "Ocelot", "Octopus", "Okapi", "Opossum", "Ostrich", "Otter", "Owl", "Oyster", "Panda", "Panther", "Parrot", "Partridge", "Peafowl", "Pelican", "Penguin", "Pheasant", "Pig", "Pigeon", "Pika", "PolarBear", "Polecat", "Pony", "Porcupine", "Porpoise", "PrairieDog", "Pug", "Puma", "Puppy", "Quail", "Quelea", "Quetzal", "Rabbit", "Raccoon", "Ram", "Rat", "Raven", "RedDeer", "RedPanda", "Reindeer", "Rhinoceros", "Rook", "Rooster", "Salamander", "Salmon", "SandDollar", "Sandpiper", "Sardine", "SeaLion", "Seahorse", "Seal", "Shark", "Sheep", "Shrew", "Siamang", "Skunk", "Sloth", "Slug", "Snail", "Snake", "Snowshoe", "Sow", "Sparrow", "Spider", "Squid", "Squirrel", "Stalion", "Starling", "Stegosaurus", "Stoat", "Swan", "Tapir", "Tarsier", "Termite", "Tiger", "Toad", "Tortoise", "Turkey", "Turtle", "Vicuna", "Viper", "Vole", "Vulture", "Wallaby", "Walrus", "Wasp", "WaterBuffalo", "Weasel", "Whale", "Wolf", "Wolverine", "Wombat", "Woodpecker", "Worm", "Wren", "Yak", "Zebra", "Zebu" ];
