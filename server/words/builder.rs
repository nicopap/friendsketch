//! Create a store for decks. The `DeckManager` is the only source of `Deck` in
//! this program. The decks are built from the directory provided to
//! `DeckManager::new`.

use super::Collection;
use crate::api::{self, Difficulty};
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use quick_error::quick_error;
use string_interner::{StringInterner, Symbol};

use std::{
    ffi::OsStr,
    fs,
    io::{self, BufRead, BufReader},
    path::{Path, PathBuf, StripPrefixError},
    str::FromStr,
};
use walkdir::{self, DirEntry, WalkDir};

quick_error! {
    #[derive(Debug)]
    pub enum DeckBuilderError {
        FileName(file: PathBuf) {
            display("the following file is not named properly: {}", file.display())
        }
        NoWords(file: PathBuf) {
            display("there are no words in {}", file.display())
        }
        InvalidPath {
            display("the path to the deck cannot be converted into the lang/difficulty/topic triplet")
        }
        NoFiles(dir: PathBuf) {
            display("there are no files in the directory for {}", dir.display())
        }
        DuplicateWord(word: String, file: PathBuf) {
            display("`{}` was found a second time in {}, it may be because it \
                    was already present in a different file!", word, file.display())
        }
        NotPrefix(err: StripPrefixError) {
            from()
        }
        WalkError(err: walkdir::Error) {
            from()
        }
        IoError(err: io::Error) {
            from()
        }
    }
}
type BuilderResult<T> = Result<T, DeckBuilderError>;
type WordSet = FxHashSet<String>;

struct TopicStash(StringInterner<Topic, FxBuildHasher>);
impl TopicStash {
    fn new() -> TopicStash {
        TopicStash(StringInterner::with_capacity_and_hasher(
            10,
            Default::default(),
        ))
    }

    fn get_or_intern(&mut self, val: impl Into<String> + AsRef<str>) -> Topic {
        self.0.get_or_intern(val)
    }

    fn resolve(&self, symbol: Topic) -> Option<&str> {
        self.0.resolve(symbol)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Topic(usize);
impl Symbol for Topic {
    fn from_usize(val: usize) -> Topic {
        Topic(val)
    }

    fn to_usize(self) -> usize {
        self.0
    }
}
pub struct Deck {
    pub(super) words:      WordSet,
    pub(super) difficulty: Difficulty,
    pub(super) topic:      Topic,
}

pub type DeckId = (Difficulty, Topic);

enum Lang {
    En,
}
impl FromStr for Lang {
    type Err = ();

    fn from_str(s: &str) -> Result<Lang, ()> {
        match s {
            "en" => Ok(Lang::En),
            _ => Err(()),
        }
    }
}

/// A memory storage that loads from the file system the available decks.
///
/// When a new `DeckManager` is created, it will
pub struct DeckManager {
    topic_stash: TopicStash,
    decks:       FxHashMap<DeckId, Deck>,
}
impl DeckManager {
    /// Attempts to load all decks present in the given `path`. If none are
    /// succesfull, returns (`None`, `error_list`). If some decks are loaded,
    /// returns the partial `DeckManager`, with the list of errors that occured
    /// but were skipped. The list is empty of no errors occured
    pub fn new(
        directory: PathBuf,
    ) -> (Option<DeckManager>, Vec<DeckBuilderError>) {
        let mut topic_stash = TopicStash::new();
        let deck_walker = WalkDir::new(&directory).min_depth(3).max_depth(3);
        let into_decks = |entry: walkdir::Result<DirEntry>| {
            let entry = entry?;
            Deck::from_directory(entry.path(), &directory, &mut topic_stash)
        };
        let mut error_list = Vec::new();
        let mut decks =
            FxHashMap::with_capacity_and_hasher(10, Default::default());
        for deck in deck_walker.into_iter().map(into_decks) {
            match deck {
                Ok(deck) => {
                    let id = (deck.difficulty, deck.topic);
                    decks.insert(id, deck);
                }
                Err(err) => {
                    error_list.push(err);
                }
            }
        }
        if decks.is_empty() {
            (None, error_list)
        } else {
            let deck_manager = DeckManager { topic_stash, decks };
            (Some(deck_manager), error_list)
        }
    }

    pub fn decks(&self) -> Vec<api::Deck> {
        let mut ret = Vec::with_capacity(self.decks.len());
        ret.extend(self.decks.keys().map(|deck_id| {
            // deck_id is a deck, therefore, unwrap doesn't panic
            let word_count = self.word_count(deck_id).unwrap();
            let &(difficulty, topic) = deck_id;
            let topic = self.api_topic(topic).unwrap();
            api::Deck {
                difficulty,
                topic,
                word_count,
            }
        }));
        ret
    }

    fn word_count(&self, deck_id: &DeckId) -> Option<u16> {
        self.decks.get(deck_id).map(|x| x.words.len() as u16)
    }

    fn api_topic(&self, topic: Topic) -> Option<api::Topic> {
        let id = api::TopicId(topic.0);
        let name = self.topic_stash.resolve(topic)?.to_string();
        let description = "".to_string();
        Some(api::Topic {
            id,
            name,
            description,
        })
    }

    pub fn to_topic(&self, topic: &api::TopicId) -> Option<Topic> {
        let topic = Topic(topic.0);
        if self.topic_stash.resolve(topic).is_some() {
            Some(topic)
        } else {
            None
        }
    }

    pub fn build_deck(
        &self,
        distrs: (f32, f32, f32),
        decks: impl Iterator<Item = DeckId>,
    ) -> Collection {
        Collection::new(&self.decks, distrs, decks)
    }
}

fn add_file_words(words: &mut WordSet, path: &Path) -> BuilderResult<()> {
    for word in BufReader::new(fs::File::open(path)?).lines() {
        let word = word?;
        if words.contains(&word) {
            let p = path.to_path_buf();
            return Err(DeckBuilderError::DuplicateWord(word, p));
        } else {
            words.insert(word);
        }
    }
    Ok(())
}

/// Separate a file path relative to the root of the words directory into its
/// rust equivalents.
fn from_path(
    deck_location: &Path,
    stash: &mut TopicStash,
) -> Result<(Difficulty, Topic), ()> {
    macro_rules! parse {
        ($val:expr, $closure:expr) => {
            $val.next()
                .and_then(|x| x.as_os_str().to_str())
                .ok_or(())
                .and_then($closure)
        };
    }
    let mut components = deck_location.components();
    let _lang: Lang = parse!(components, str::parse)?;
    let difficulty = parse!(components, str::parse)?;
    let topic = parse!(components, |x| Ok(stash.get_or_intern(x)))?;
    Ok((difficulty, topic))
}

impl Deck {
    /// Collect all words that located in multiple .txt files in the given
    /// directory.
    fn from_directory(
        path: &Path,
        prefix: &Path,
        stash: &mut TopicStash,
    ) -> BuilderResult<Deck> {
        let (difficulty, topic) = from_path(path.strip_prefix(prefix)?, stash)
            .map_err(|_| DeckBuilderError::InvalidPath)?;
        let mut words =
            WordSet::with_capacity_and_hasher(100, Default::default());
        let readable = |file: &Result<DirEntry, _>| {
            file.as_ref()
                .map(|x| x.file_type().is_file())
                .unwrap_or(false)
        };
        for entry in WalkDir::new(path).into_iter().filter(readable) {
            let entry = entry?;
            let entry_path = entry.path();
            if entry_path.extension().and_then(OsStr::to_str) == Some("txt") {
                add_file_words(&mut words, entry_path)?;
            } else {
                return Err(DeckBuilderError::FileName(
                    entry_path.to_path_buf(),
                ));
            }
        }
        if words.is_empty() {
            let buff_path = PathBuf::from(path);
            Err(DeckBuilderError::NoFiles(buff_path))
        } else {
            Ok(Deck {
                words,
                difficulty,
                topic,
            })
        }
    }
}
