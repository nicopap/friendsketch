use super::{
    builder::{Deck, DeckId},
    Topic,
};
use crate::api::Difficulty;
use fxhash::FxHashMap;
use itertools::Itertools;
use log::debug;
use rand::{
    distributions::{Distribution, WeightedIndex},
    seq::{index, IteratorRandom, SliceRandom},
    thread_rng,
};
use std::fmt;

struct Words<'a> {
    weight:        f32,
    words:         Vec<(Topic, &'a str)>,
    indices:       index::IndexVec,
    current_index: usize,
}
impl<'a> Words<'a> {
    fn new(weight: f32, words: Vec<(Topic, &str)>) -> Words {
        let len = words.len();
        let indices = index::sample(&mut thread_rng(), len, len);
        let words: Vec<_> =
            words.into_iter().unique_by(|elem| elem.1).collect();
        let weight = if words.is_empty() { 0.0 } else { weight };
        Words {
            weight,
            words,
            indices,
            current_index: 0,
        }
    }
}
impl<'a> Iterator for Words<'a> {
    type Item = (Topic, Word<'a>);

    fn next(&mut self) -> Option<(Topic, Word<'a>)> {
        let len = self.indices.len();
        if len == 0 {
            return None;
        } else if self.current_index >= len {
            self.indices = index::sample(&mut thread_rng(), len, len);
            self.current_index = 0;
        };
        let random_index = self.indices.index(self.current_index);
        let ret = self.words.get(random_index).map(|&(topic, word)| {
            let word = Word::new(word);
            (topic, word)
        });
        self.current_index += 1;
        ret
    }
}
impl<'a> fmt::Display for Words<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut display = String::with_capacity(1000);
        for (_, x) in self.words.iter() {
            let to_push = format!("{}, ", x);
            display.push_str(&to_push);
        }
        f.pad(&display)
    }
}

/// Panics if sum of weights is negative or equals to 0
fn generate_hits((e, m, h): (f32, f32, f32)) -> [Difficulty; 16] {
    use self::Difficulty::*;
    macro_rules! collect_all {
        ($distr:expr; $($coma:tt)+) => (
            [ $($distr.next().unwrap() $coma)+ ]
        );
    }
    let weights = [e, m, h];
    let distribution = WeightedIndex::new(&weights).unwrap();
    let choose = |x| match x {
        0 => Easy,
        1 => Normal,
        2 => Hard,
        _ => unreachable!(),
    };
    let mut rng = thread_rng();
    let mut distr = distribution.sample_iter(&mut rng).map(choose);
    collect_all!(distr; ,,,,,,,,,,,,,,,,)
}

pub struct Collection<'a> {
    easy:         Words<'a>,
    normal:       Words<'a>,
    hard:         Words<'a>,
    next_hits:    [Difficulty; 16],
    hits_pointer: u8,
}
impl<'a> Collection<'a> {
    pub(super) fn new(
        decks: &FxHashMap<DeckId, Deck>,
        distr: (f32, f32, f32),
        d: impl Iterator<Item = DeckId>,
    ) -> Collection {
        let mut easy_words = Vec::new();
        let mut normal_words = Vec::new();
        let mut hard_words = Vec::new();
        for deck_id in d {
            if let Some(deck) = decks.get(&deck_id) {
                let target = match deck_id.0 {
                    Difficulty::Easy => &mut easy_words,
                    Difficulty::Normal => &mut normal_words,
                    Difficulty::Hard => &mut hard_words,
                };
                let words = &deck.words;
                target.extend(
                    words.iter().map(|word| (deck_id.1, word.as_ref())),
                );
            }
        }
        let easy = Words::new(distr.0, easy_words);
        let normal = Words::new(distr.1, normal_words);
        let hard = Words::new(distr.2, hard_words);
        let distr = (easy.weight, normal.weight, hard.weight);
        let next_hits = generate_hits(distr);
        Collection {
            easy,
            normal,
            hard,
            next_hits,
            hits_pointer: 0,
        }
    }
}
impl<'a> Iterator for Collection<'a> {
    type Item = (Difficulty, Topic, Word<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        use self::Difficulty::*;
        let this_hit = self.next_hits[usize::from(self.hits_pointer)];
        debug!("{:?}", this_hit);
        let ret = match this_hit {
            Easy => self.easy.next(),
            Normal => self.normal.next(),
            Hard => self.hard.next(),
        }
        .map(|(a, b)| (this_hit, a, b));
        if let Some((_, _, ref word)) = ret {
            debug!("selected: {}", word);
        } else {
            debug!("none selected D:");
        }
        self.hits_pointer += 1;
        if self.hits_pointer >= 16 {
            let (e, m, h) =
                (self.easy.weight, self.normal.weight, self.hard.weight);
            self.next_hits = generate_hits((e, m, h));
            self.hits_pointer = 0;
        }
        ret
    }
}
impl<'a> fmt::Debug for Collection<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let display = format!(
            "easy: {}\nnormal: {}\nhard: {}",
            self.easy, self.normal, self.hard
        );
        f.pad(&display)
    }
}

#[derive(Clone)]
pub struct Word<'a> {
    word:       &'a str,
    char_count: u8,
    reveals:    Vec<(u8, char)>,
}
impl<'a> Word<'a> {
    fn new(word: &str) -> Word {
        let mut rng = thread_rng();
        let char_count = word.chars().count() as u8;
        let mut reveals = word
            .chars()
            .enumerate()
            .map(|(i, e)| (i as u8, e))
            .choose_multiple(&mut rng, 8);
        reveals.shuffle(&mut thread_rng());
        Word {
            word,
            char_count,
            reveals,
        }
    }

    pub fn next_letter(&mut self) -> Option<(u8, char)> {
        self.reveals.pop()
    }

    pub fn len(&self) -> usize {
        usize::from(self.char_count)
    }
}
impl<'a> PartialEq<[u8]> for &Word<'a> {
    fn eq(&self, other: &[u8]) -> bool {
        self.word.as_bytes() == other
    }
}
impl<'a> PartialEq<str> for &Word<'a> {
    fn eq(&self, other: &str) -> bool {
        self.word == other
    }
}
impl<'a> fmt::Display for Word<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad(self.word)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_duplicates() {
        panic!("")
    }
}
