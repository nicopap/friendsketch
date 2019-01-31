use super::game::{self, JoinResponse, LeaveResponse};
use crate::api::{
    self, ChatMsg, ClassicMsg, GameEvent, GameState, Name, Stroke,
};
use arraydeque::{ArrayDeque, Wrapping};
use log::{error, info, warn};
use rand::seq::IteratorRandom;
use slotmap::{new_key_type, SlotMap};
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
        room_leader: Id,
    },
    /// Result screen for the previous round where `artist` was the artist
    RoundResults {
        artist: Id,
    },
    Playing {
        drawing: Vec<Stroke>,
        artist: Id,
        word: &'static str,
        round_start_time: Instant,
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
    game_log:    ArrayDeque<[GameEvent; 20], Wrapping>,
    round_count: u8,
    round_no:    u8,
}

#[derive(Clone)]
enum RoundScore {
    Failed,
    Absent,
    Artist(u16),
    Guessed(u16),
}
impl Into<api::RoundSummary> for RoundScore {
    fn into(self) -> api::RoundSummary {
        use self::RoundScore::*;
        match self {
            Failed => api::RoundSummary::Failed,
            Absent => api::RoundSummary::Absent,
            Artist(score) => api::RoundSummary::Artist(score),
            Guessed(score) => api::RoundSummary::Guessed(score),
        }
    }
}

#[derive(Clone)]
struct Player {
    name:        Name,
    score:       Vec<RoundScore>,
    has_guessed: bool,
}
impl Into<(Name, api::RoundSummary)> for Player {
    fn into(self) -> (Name, api::RoundSummary) {
        let Player {
            name, mut score, ..
        } = self;
        let score = score.pop().unwrap_or(RoundScore::Absent).into();
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

#[rustfmt::skip]
static WORD_LIST: [&str; 256] = [ "Aardvark", "Albatross", "Alligator", "Alpaca", "Ant", "Anteater", "Antelope", "Ape", "Armadillo", "Baboon", "Badger", "Barracuda", "Bat", "Bear", "Beaver", "Bee", "Beetle", "Bird", "Bison", "Boar", "Bobcat", "Buffalo", "Bull", "Butterfly", "Camel", "Caribou", "Cassowary", "Cat", "Caterpillar", "Cattle", "Chameleon", "Chamois", "Cheetah", "Chicken", "Chimpanzee", "Chinchilla", "Chipmunk", "Chough", "Civet", "Coati", "Cobra", "Cockroach", "Cod", "Cormorant", "Cougar", "Cow", "Coyote", "Crab", "Crane", "Crocodile", "Crow", "Cuckoo", "Curlew", "Deer", "Dinosaur", "Doe", "Dog", "Dogfish", "Dolphin", "Donkey", "Dotterel", "Dove", "Dragonfly", "Dromedary", "Duck", "Dugong", "Dunlin", "Eagle", "Echidna", "Eel", "Eland", "Elephant", "ElephantSeal", "Elk", "Emu", "Falcon", "Ferret", "Finch", "Fish", "Flamingo", "Fly", "Fox", "Frog", "Gaur", "Gazelle", "Gerbil", "GiantPanda", "Giraffe", "Gnat", "Gnu", "Goat", "Goldfinch", "Goosander", "Goose", "Gorilla", "Goshawk", "Grasshopper", "Grouse", "Guanaco", "GuineaFowl", "GuineaPig", "Gull", "Hamster", "Hare", "Hawk", "Hedgehog", "Heron", "Herring", "Hippo", "Hornet", "Horse", "Hummingbird", "Hyena", "Ibex", "Ibis", "Impala", "Jackal", "Jaguar", "Jay", "Jellyfish", "Kangaroo", "Kinkajou", "Kitten", "Kiwi", "Koala", "KomodoDragon", "Kouprey", "Kudu", "Ladybug", "Lapwing", "Lark", "Lemur", "Leopard", "Lion", "Lizard", "Llama", "Lobster", "Locust", "Loris", "Louse", "Lynx", "Lyrebird", "Magpie", "Mallard", "Mammoth", "Manatee", "Mandrill", "Marmoset", "Mink", "Mole", "Mongoose", "Monkey", "Moose", "Mosquito", "Moth", "Mouse", "Narwhal", "Newt", "Nightlingale", "Ocelot", "Octopus", "Okapi", "Opossum", "Ostrich", "Otter", "Owl", "Oyster", "Panda", "Panther", "Parrot", "Partridge", "Peafowl", "Pelican", "Penguin", "Pheasant", "Pig", "Pigeon", "Pika", "PolarBear", "Polecat", "Pony", "Porcupine", "Porpoise", "PrairieDog", "Pug", "Puma", "Puppy", "Quail", "Quelea", "Quetzal", "Rabbit", "Raccoon", "Ram", "Rat", "Raven", "RedDeer", "RedPanda", "Reindeer", "Rhinoceros", "Rook", "Rooster", "Salamander", "Salmon", "SandDollar", "Sandpiper", "Sardine", "SeaLion", "Seahorse", "Seal", "Shark", "Sheep", "Shrew", "Siamang", "Skunk", "Sloth", "Slug", "Snail", "Snake", "Snowshoe", "Sow", "Sparrow", "Spider", "Squid", "Squirrel", "Stalion", "Starling", "Stegosaurus", "Stoat", "Swan", "Tapir", "Tarsier", "Termite", "Tiger", "Toad", "Tortoise", "Turkey", "Turtle", "Vicuna", "Viper", "Vole", "Vulture", "Wallaby", "Walrus", "Wasp", "WaterBuffalo", "Weasel", "Whale", "Wolf", "Wolverine", "Wombat", "Woodpecker", "Worm", "Wren", "Yak", "Zebra", "Zebu" ];

const ROUND_LENGTH: i16 = 20;

const TICK_UPDATE: Duration = Duration::from_secs(2);

fn sync_msg(
    state: GameState,
    events: impl Iterator<Item = GameEvent>,
) -> api::GameMsg {
    use api::{GameMsg::Info, InfoMsg::Sync_};
    Info(Sync_(state, events.collect()))
}

impl Game {
    fn into_sync_msg(&self) -> api::GameMsg {
        use self::GameState::{Lobby, Round};
        macro_rules! list {
            ($map:expr) => {
                self.players.iter().map($map).collect();
            };
        }
        let state = match self.state {
            Game_::Empty => {
                error!("Attempt to sync to empty state");
                panic!("tugofsketch:{} unreachable path", line!())
            }
            Game_::Lobby { room_leader } => {
                let players = list!(|(_, p)| p.name.clone());
                let master = self.players[room_leader].name.clone();
                Lobby { players, master }
            }
            Game_::RoundResults { artist, .. } => {
                error!("Unimplemented RoundResults sync");
                let players = list!(|(_, p)| p.name.clone());
                let master = self.players[artist].name.clone();
                Lobby { players, master }
            }
            Game_::Playing {
                ref drawing,
                artist,
                round_start_time,
                ..
            } => {
                let round_state = api::RoundState {
                    players: list!(|(_, p)| (p.name.clone(), vec![])),
                    artist:  self.players[artist].name.clone(),
                    timeout: ROUND_LENGTH
                        - round_start_time.elapsed().as_secs() as i16,
                };
                Round(drawing.clone(), round_state)
            }
        };
        let events = self.game_log.clone().into_iter();
        sync_msg(state, events)
    }

    /// Initiate a new round, with new artist, new word etc
    fn start_round(
        &mut self,
        instigator: Option<Id>,
    ) -> Option<(Broadcast, Cmd)> {
        use self::api::{
            GameMsg,
            Guess::{Artist, Guess},
            RoundStart,
        };
        macro_rules! start_round {
            ($next_artist:expr) => {{
                info!("Starting round {}", self.round_no);
                self.players.iter_mut().for_each(|(id, player)| {
                    player.score.push(RoundScore::Absent);
                    player.has_guessed = id == $next_artist;
                });
                let word = WORD_LIST[rand::random::<u8>() as usize];
                self.state = Game_::Playing {
                    drawing: vec![],
                    artist: $next_artist,
                    word,
                    round_start_time: Instant::now(),
                };
                let make_msg = |word| {
                    GameMsg::Classic(ClassicMsg::Start(RoundStart {
                        timeout: ROUND_LENGTH,
                        artist: self.players[$next_artist].name.clone(),
                        word,
                    }))
                };
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
            Game_::Lobby { room_leader } if instigator == Some(room_leader) => {
                self.round_no = 0;
                start_round!(room_leader)
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

    fn player_scores(&self) -> impl Iterator<Item = (Name, api::RoundSummary)> {
        self.players
            .clone()
            .into_iter()
            .map(|(_, player)| player.into())
    }

    /// Update players scores accordingly. Returns whehter all players guessed
    /// correctly
    fn guesses_correctly(&mut self, player: Id) -> bool {
        let round = self.round_no as usize;
        let is_first = self.players.iter().any(|(_, p)| {
            if let RoundScore::Guessed(_) = p.score[round] {
                true
            } else {
                false
            }
        });
        match self.state {
            Game_::Playing { artist, .. } if artist != player => {
                if let Some(to_change) = self.players.get_mut(player) {
                    if !to_change.has_guessed {
                        let score = if is_first { 3 } else { 1 };
                        to_change.score[round] = RoundScore::Guessed(score);
                        to_change.has_guessed = true;
                    }
                }
            }
            _ => error!("A guess occured outside of a regular play time"),
        }
        self.players.iter().all(|(_, p)| p.has_guessed)
    }

    /// Set game state to "Round summary" and indicate that all players who
    /// haven't guessed yet have failed
    fn end_round(&mut self) {
        info!("Ending round {}", self.round_no);
        let mut total_scores = 0;
        let round = self.round_no as usize;
        self.players.iter_mut().for_each(|(_, player)| {
            if let RoundScore::Guessed(x) = player.score[round] {
                total_scores += x
            } else {
                player.score[round] = RoundScore::Failed
            }
        });
        if let Game_::Playing { artist, .. } = self.state {
            self.players[artist].score[round] =
                RoundScore::Artist(total_scores);
            self.state = Game_::RoundResults { artist };
        } else {
            error!("Ending a round that is not happening");
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
            use self::Game_::*;
            self.game_log.push_front(GameEvent::Joined(name.clone()));
            let rounds = self.round_no as usize;
            let rounds_elapsed: usize = match self.state {
                Playing { .. } | RoundResults { .. } => rounds + 1,
                Empty | Lobby { .. } => rounds,
            };
            let new_score =
                repeat(RoundScore::Absent).take(rounds_elapsed).collect();
            let id = self.players.insert(Player {
                name,
                score: new_score,
                has_guessed: false,
            });
            match self.state {
                Game_::Empty => {
                    self.state = Game_::Lobby { room_leader: id };
                }
                Game_::Lobby { .. } => {}
                Game_::Playing { .. } => {}
                Game_::RoundResults { .. } => {}
            };
            JoinResponse::Accept(id)
        }
    }

    fn leaves(&mut self, player: Id) -> LeaveResponse<Id, api::GameMsg, ()> {
        use self::api::{
            ClassicMsg::Over,
            GameMsg::{Classic, Info},
            InfoMsg::Mastery,
        };
        match self.players.remove(player) {
            Some(Player { name, .. }) => {
                self.game_log.push_front(GameEvent::Left(name.clone()));
                if let Some((id, _)) = self.players.iter().next() {
                    let response = match self.state {
                        Game_::Lobby {
                            ref mut room_leader,
                        } if *room_leader == player => {
                            *room_leader = id;
                            broadcast!(to_unique, id, Info(Mastery))
                        }
                        Game_::Playing {
                            ref mut artist,
                            word,
                            ..
                        } if *artist == player => {
                            *artist = id;
                            let msg = Classic(Over(word.to_string(), vec![]));
                            broadcast!(to_all, msg)
                        }
                        _ => broadcast!(nothing),
                    };
                    if self.players.len() == 1 {
                        self.state = Game_::Lobby { room_leader: id };
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
        use self::api::{GameMsg, GameReq, InfoRequest};
        let msg = match request {
            GameReq::Info(InfoRequest::Sync_) => None,
            GameReq::Info(InfoRequest::Start) => self.start_round(Some(player)),
            GameReq::Canvas(msg) => {
                // TODO: write this better
                if let Game_::Playing {
                    ref mut drawing,
                    artist,
                    ..
                } = self.state
                {
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
                } else {
                    None
                }
            }
            GameReq::Chat(content) => {
                // TODO: write this better
                if let Game_::Playing { artist, word, .. } = self.state {
                    if player != artist && word.as_bytes() == content.as_bytes()
                    {
                        use api::{
                            ClassicMsg::{Correct, Guessed},
                            GameMsg::Classic,
                        };
                        let all_guessed = self.guesses_correctly(player);
                        let guesser = self.players[player].name.clone();
                        let msg = Classic(Guessed(guesser));
                        let correct = Some(Classic(Correct(word.to_string())));
                        let cmd = if all_guessed {
                            let end_round = Feedback {
                                sent_round: self.round_no,
                                msg:        Feedback_::EndRound,
                            };
                            game::Cmd::Immediately(end_round)
                        } else {
                            game::Cmd::None
                        };
                        Some(Some((
                            broadcast!(to_all_but, player, msg, correct),
                            cmd,
                        )))
                    } else {
                        None
                    }
                } else {
                    None
                }
                .unwrap_or_else(|| {
                    let final_msg = ChatMsg {
                        content,
                        author: self.players[player].name.clone(),
                    };
                    let log_item = GameEvent::Message(final_msg.clone());
                    self.game_log.push_front(log_item);
                    Some((
                        broadcast!(to_all, GameMsg::Chat(final_msg)),
                        game::Cmd::None,
                    ))
                })
            }
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
            api::{ClassicMsg, GameMsg::Classic},
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
                let msg = Classic(ClassicMsg::Reveal(index, letter));
                Ok((
                    broadcast!(to_all_but, *artist, msg, None),
                    game::Cmd::None,
                ))
            }
            (Playing { word, .. }, EndRound) => {
                let word = word.to_string();
                self.end_round();
                let send =
                    ClassicMsg::Over(word, self.player_scores().collect());
                let msg = Feedback_::NextRound;
                let cmd = game::Cmd::In(
                    Duration::from_secs(3),
                    Feedback { sent_round, msg },
                );
                Ok((broadcast!(to_all, Classic(send)), cmd))
            }
            (
                Playing {
                    round_start_time,
                    word,
                    ..
                },
                TickTimeout,
            ) => {
                let timeout =
                    ROUND_LENGTH - round_start_time.elapsed().as_secs() as i16;
                let (classic_reply, cmd) = if timeout <= 0 {
                    let word = word.to_string();
                    self.end_round();
                    let send =
                        ClassicMsg::Over(word, self.player_scores().collect());
                    let msg = Feedback_::NextRound;
                    let cmd = game::Cmd::In(
                        Duration::from_secs(3),
                        Feedback { sent_round, msg },
                    );
                    (Classic(send), cmd)
                } else {
                    let send = ClassicMsg::TimeoutSync(timeout);
                    let msg = Feedback_::TickTimeout;
                    let cmd = game::Cmd::In(
                        TICK_UPDATE,
                        Feedback { sent_round, msg },
                    );
                    (Classic(send), cmd)
                };
                Ok((broadcast!(to_all, classic_reply), cmd))
            }
            (RoundResults { .. }, NextRound) => {
                Ok(self.start_round(None).unwrap())
            }
            (_state, feedback) => {
                error!("invalid feedback: {:?} for game state", feedback);
                Ok((broadcast!(nothing), game::Cmd::None))
            }
        }
    }
}
