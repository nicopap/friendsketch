//! Manage decks and build collections of decks
mod builder;
mod collection;

pub use builder::{DeckManager, Topic};
pub use collection::{Collection, Word};
