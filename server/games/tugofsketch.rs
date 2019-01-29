use super::game::{self, JoinResponse, LeaveResponse, TellResponse};
use crate::api::{
    self, ChatMsg, ClassicMsg, GameEvent, GameState, Name, Stroke,
};
use arraydeque::{ArrayDeque, Wrapping};
use log::{error, warn};
use slotmap::{new_key_type, SlotMap};
use std::time::Instant;

type Slab<T> = SlotMap<Id, T>;

new_key_type! {
    pub struct Id;
}

enum Game_ {
    Empty,
    Lobby {
        room_leader: Id,
    },
    Playing {
        drawing: Vec<Stroke>,
        artist: Id,
        word: &'static str,
        round_start_time: Instant,
    },
}

pub struct Game {
    state:    Game_,
    players:  Slab<Player>,
    game_log: ArrayDeque<[GameEvent; 20], Wrapping>,
}

struct Player {
    name: Name,
}

macro_rules! broadcast {
    (to_all, $msg:expr) => {
        TellResponse::ToAll($msg)
    };
    (to, $players:expr, $msg:expr) => {
        TellResponse::ToList($players.collect(), $msg)
    };
    (to_all_but, $player:expr, $msg:expr, $optmsg:expr) => {
        TellResponse::ToAllBut($player, $msg, $optmsg)
    };
    (to_unique, $player:expr, $msg:expr) => {
        TellResponse::ToList(vec![$player], $msg)
    };
    (nothing) => {
        TellResponse::ToNone
    };
}

#[rustfmt::skip]
static WORD_LIST: [&str; 256] = [ "Aardvark", "Albatross", "Alligator", "Alpaca", "Ant", "Anteater", "Antelope", "Ape", "Armadillo", "Baboon", "Badger", "Barracuda", "Bat", "Bear", "Beaver", "Bee", "Beetle", "Bird", "Bison", "Boar", "Bobcat", "Buffalo", "Bull", "Butterfly", "Camel", "Caribou", "Cassowary", "Cat", "Caterpillar", "Cattle", "Chameleon", "Chamois", "Cheetah", "Chicken", "Chimpanzee", "Chinchilla", "Chipmunk", "Chough", "Civet", "Coati", "Cobra", "Cockroach", "Cod", "Cormorant", "Cougar", "Cow", "Coyote", "Crab", "Crane", "Crocodile", "Crow", "Cuckoo", "Curlew", "Deer", "Dinosaur", "Doe", "Dog", "Dogfish", "Dolphin", "Donkey", "Dotterel", "Dove", "Dragonfly", "Dromedary", "Duck", "Dugong", "Dunlin", "Eagle", "Echidna", "Eel", "Eland", "Elephant", "ElephantSeal", "Elk", "Emu", "Falcon", "Ferret", "Finch", "Fish", "Flamingo", "Fly", "Fox", "Frog", "Gaur", "Gazelle", "Gerbil", "GiantPanda", "Giraffe", "Gnat", "Gnu", "Goat", "Goldfinch", "Goosander", "Goose", "Gorilla", "Goshawk", "Grasshopper", "Grouse", "Guanaco", "GuineaFowl", "GuineaPig", "Gull", "Hamster", "Hare", "Hawk", "Hedgehog", "Heron", "Herring", "Hippo", "Hornet", "Horse", "Hummingbird", "Hyena", "Ibex", "Ibis", "Impala", "Jackal", "Jaguar", "Jay", "Jellyfish", "Kangaroo", "Kinkajou", "Kitten", "Kiwi", "Koala", "KomodoDragon", "Kouprey", "Kudu", "Ladybug", "Lapwing", "Lark", "Lemur", "Leopard", "Lion", "Lizard", "Llama", "Lobster", "Locust", "Loris", "Louse", "Lynx", "Lyrebird", "Magpie", "Mallard", "Mammoth", "Manatee", "Mandrill", "Marmoset", "Mink", "Mole", "Mongoose", "Monkey", "Moose", "Mosquito", "Moth", "Mouse", "Narwhal", "Newt", "Nightlingale", "Ocelot", "Octopus", "Okapi", "Opossum", "Ostrich", "Otter", "Owl", "Oyster", "Panda", "Panther", "Parrot", "Partridge", "Peafowl", "Pelican", "Penguin", "Pheasant", "Pig", "Pigeon", "Pika", "PolarBear", "Polecat", "Pony", "Porcupine", "Porpoise", "PrairieDog", "Pug", "Puma", "Puppy", "Quail", "Quelea", "Quetzal", "Rabbit", "Raccoon", "Ram", "Rat", "Raven", "RedDeer", "RedPanda", "Reindeer", "Rhinoceros", "Rook", "Rooster", "Salamander", "Salmon", "SandDollar", "Sandpiper", "Sardine", "SeaLion", "Seahorse", "Seal", "Shark", "Sheep", "Shrew", "Siamang", "Skunk", "Sloth", "Slug", "Snail", "Snake", "Snowshoe", "Sow", "Sparrow", "Spider", "Squid", "Squirrel", "Stalion", "Starling", "Stegosaurus", "Stoat", "Swan", "Tapir", "Tarsier", "Termite", "Tiger", "Toad", "Tortoise", "Turkey", "Turtle", "Vicuna", "Viper", "Vole", "Vulture", "Wallaby", "Walrus", "Wasp", "WaterBuffalo", "Weasel", "Whale", "Wolf", "Wolverine", "Wombat", "Woodpecker", "Worm", "Wren", "Yak", "Zebra", "Zebu" ];

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
            Game_::Playing {
                ref drawing,
                artist,
                round_start_time,
                ..
            } => {
                let players = list!(|(_, p)| (p.name.clone(), vec![]));
                let artist = self.players[artist].name.clone();
                let timeout = 90 - round_start_time.elapsed().as_secs() as u16;
                let round_state = api::RoundState {
                    players,
                    artist,
                    timeout,
                };
                Round(drawing.clone(), round_state)
            }
        };
        let events = self.game_log.clone().into_iter();
        sync_msg(state, events)
    }
}
impl game::Game<Id> for Game {
    type Error = ();
    type Request = api::GameReq;
    type Response = api::GameMsg;

    fn new() -> Self {
        Game {
            state:    Game_::Empty,
            players:  Slab::with_key(),
            game_log: ArrayDeque::new(),
        }
    }

    fn joins(&mut self, name: Name) -> JoinResponse<Id> {
        if self.players.values().any(|Player { name: n }| n == &name) {
            JoinResponse::Refuse
        } else {
            self.game_log.push_front(GameEvent::Joined(name.clone()));
            let id = self.players.insert(Player { name });
            match self.state {
                Game_::Empty => {
                    self.state = Game_::Lobby { room_leader: id };
                }
                Game_::Lobby { .. } => {}
                Game_::Playing { .. } => {}
            };
            JoinResponse::Accept(id)
        }
    }

    fn leaves(&mut self, player: Id) -> LeaveResponse<Id, api::GameMsg, ()> {
        use self::api::{GameMsg::Info, InfoMsg::Mastery};
        match self.players.remove(player) {
            Some(Player { name }) => {
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
                            let msg = api::GameMsg::Classic(ClassicMsg::Over(
                                word.to_string(),
                                vec![],
                            ));
                            broadcast!(to_all, msg)
                        }
                        _ => broadcast!(nothing),
                    };
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
    ) -> Result<TellResponse<Id, api::GameMsg>, ()> {
        use self::api::{GameMsg, GameReq, InfoRequest};
        Ok(match request {
            GameReq::Info(InfoRequest::Sync_) => None,
            GameReq::Info(InfoRequest::Start) => {
                if let Game_::Lobby { room_leader } = self.state {
                    if player != room_leader {
                        warn!("{:?} tries to start, but isn't leader", player);
                        None
                    } else {
                        use self::api::{
                            Guess::{Artist, Guess},
                            RoundStart,
                        };
                        let round_start_time = Instant::now();
                        let word = WORD_LIST[rand::random::<u8>() as usize];
                        self.state = Game_::Playing {
                            drawing: vec![],
                            artist: room_leader,
                            word,
                            round_start_time,
                        };
                        let make_msg = |word| {
                            GameMsg::Classic(ClassicMsg::Start(RoundStart {
                                timeout: 90,
                                artist: self.players[room_leader].name.clone(),
                                word,
                            }))
                        };
                        let word_length = word.chars().count();
                        let guessers_msg = make_msg(Guess(word_length));
                        let artist_msg = make_msg(Artist(String::from(word)));
                        Some(broadcast!(
                            to_all_but,
                            room_leader,
                            guessers_msg,
                            Some(artist_msg)
                        ))
                    }
                } else {
                    warn!("{:?} tries to start while game is running", player);
                    None
                }
            }
            GameReq::Canvas(msg) => {
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
                        Some(broadcast!(to_all_but, artist, msg, None))
                    }
                } else {
                    None
                }
            }
            GameReq::Chat(content) => if let Game_::Playing {
                ref artist,
                ref word,
                ..
            } = self.state
            {
                if player != *artist && word.as_bytes() == content.as_bytes() {
                    use api::{
                        ClassicMsg::{Correct, Guessed},
                        GameMsg::Classic,
                    };
                    let guesser = self.players[player].name.clone();
                    let msg = Classic(Guessed(guesser));
                    let correct = Some(Classic(Correct(word.to_string())));
                    Some(Some(broadcast!(to_all_but, player, msg, correct)))
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
                Some(broadcast!(to_all, GameMsg::Chat(final_msg)))
            }),
        }
        .unwrap_or_else(|| broadcast!(to_unique, player, self.into_sync_msg())))
    }
}
