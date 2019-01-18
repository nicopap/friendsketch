use fxhash::FxHashMap;
use lazy_static::lazy_static;

#[rustfmt::skip]
pub static LIST: [&str;256] = [ "Aardvark", "Albatross", "Alligator", "Alpaca", "Ant", "Anteater", "Antelope", "Ape", "Armadillo", "Baboon", "Badger", "Barracuda", "Bat", "Bear", "Beaver", "Bee", "Beetle", "Bird", "Bison", "Boar", "Bobcat", "Buffalo", "Bull", "Butterfly", "Camel", "Caribou", "Cassowary", "Cat", "Caterpillar", "Cattle", "Chameleon", "Chamois", "Cheetah", "Chicken", "Chimpanzee", "Chinchilla", "Chipmunk", "Chough", "Civet", "Coati", "Cobra", "Cockroach", "Cod", "Cormorant", "Cougar", "Cow", "Coyote", "Crab", "Crane", "Crocodile", "Crow", "Cuckoo", "Curlew", "Deer", "Dinosaur", "Doe", "Dog", "Dogfish", "Dolphin", "Donkey", "Dotterel", "Dove", "Dragonfly", "Dromedary", "Duck", "Dugong", "Dunlin", "Eagle", "Echidna", "Eel", "Eland", "Elephant", "ElephantSeal", "Elk", "Emu", "Falcon", "Ferret", "Finch", "Fish", "Flamingo", "Fly", "Fox", "Frog", "Gaur", "Gazelle", "Gerbil", "GiantPanda", "Giraffe", "Gnat", "Gnu", "Goat", "Goldfinch", "Goosander", "Goose", "Gorilla", "Goshawk", "Grasshopper", "Grouse", "Guanaco", "GuineaFowl", "GuineaPig", "Gull", "Hamster", "Hare", "Hawk", "Hedgehog", "Heron", "Herring", "Hippo", "Hornet", "Horse", "Hummingbird", "Hyena", "Ibex", "Ibis", "Impala", "Jackal", "Jaguar", "Jay", "Jellyfish", "Kangaroo", "Kinkajou", "Kitten", "Kiwi", "Koala", "KomodoDragon", "Kouprey", "Kudu", "Ladybug", "Lapwing", "Lark", "Lemur", "Leopard", "Lion", "Lizard", "Llama", "Lobster", "Locust", "Loris", "Louse", "Lynx", "Lyrebird", "Magpie", "Mallard", "Mammoth", "Manatee", "Mandrill", "Marmoset", "Mink", "Mole", "Mongoose", "Monkey", "Moose", "Mosquito", "Moth", "Mouse", "Narwhal", "Newt", "Nightlingale", "Ocelot", "Octopus", "Okapi", "Opossum", "Ostrich", "Otter", "Owl", "Oyster", "Panda", "Panther", "Parrot", "Partridge", "Peafowl", "Pelican", "Penguin", "Pheasant", "Pig", "Pigeon", "Pika", "PolarBear", "Polecat", "Pony", "Porcupine", "Porpoise", "PrairieDog", "Pug", "Puma", "Puppy", "Quail", "Quelea", "Quetzal", "Rabbit", "Raccoon", "Ram", "Rat", "Raven", "RedDeer", "RedPanda", "Reindeer", "Rhinoceros", "Rook", "Rooster", "Salamander", "Salmon", "SandDollar", "Sandpiper", "Sardine", "SeaLion", "Seahorse", "Seal", "Shark", "Sheep", "Shrew", "Siamang", "Skunk", "Sloth", "Slug", "Snail", "Snake", "Snowshoe", "Sow", "Sparrow", "Spider", "Squid", "Squirrel", "Stalion", "Starling", "Stegosaurus", "Stoat", "Swan", "Tapir", "Tarsier", "Termite", "Tiger", "Toad", "Tortoise", "Turkey", "Turtle", "Vicuna", "Viper", "Vole", "Vulture", "Wallaby", "Walrus", "Wasp", "WaterBuffalo", "Weasel", "Whale", "Wolf", "Wolverine", "Wombat", "Woodpecker", "Worm", "Wren", "Yak", "Zebra", "Zebu", ];

lazy_static! {
    static ref FXHASH: FxHashMap<&'static str, u8> = {
        let mut m = FxHashMap::default();
        for (i, &name) in LIST.iter().enumerate() {
            m.insert(name, i as u8);
        }
        m
    };
}

pub fn from(animal: &str) -> Option<u8> {
    FXHASH.get(&animal).map(|&x| x)
}
