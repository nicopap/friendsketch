//! Manage game rooms and dynamically generated endpoints

use crate::{
    api::{Name, RoomId},
    games::{GameRoom, Id, ManagerResponse},
};
use chashmap::CHashMap;
use log::info;
use quick_error::quick_error;
use slotmap::{new_key_type, SlotMap};
use std::{
    str::FromStr,
    sync::{Arc, RwLock},
};
use warp::{filters::ws::Ws2, reply::Reply};

type Slab<T> = SlotMap<ConnId, T>;
new_key_type! {
    pub struct ConnId;
}

quick_error! {
    #[derive(Debug)]
    pub enum ConnError {
        NotFound {
            description("The id led to a room, but that room doesn't exist")
        }
        NotTracked {
            description("The given connection id doesn't correspond to an existing room")
        }
        BadBase64(err: base64::DecodeError) {
            from()
        }
        BadBincode(err: bincode::Error) {
            from()
        }
    }
}

fn jamble_bytes(bytes: &mut [u8]) {
    let jambler = [90, 151, 33, 44, 212, 207, 18, 151];
    bytes
        .iter_mut()
        .enumerate()
        .for_each(|(i, x)| *x ^= jambler[i % 8]);
}
impl ConnId {
    pub fn into_url(self) -> String {
        let mut byte_buff = bincode::serialize(&self).unwrap();
        jamble_bytes(&mut byte_buff);
        base64::encode_config(&byte_buff, base64::URL_SAFE_NO_PAD)
    }
}
impl FromStr for ConnId {
    type Err = ConnError;

    fn from_str(s: &str) -> Result<ConnId, ConnError> {
        let mut byte_buff = base64::decode_config(s, base64::URL_SAFE_NO_PAD)?;
        jamble_bytes(&mut byte_buff);
        bincode::deserialize(&byte_buff).map_err(Into::into)
    }
}

struct ConnIdStore<T>(RwLock<Slab<T>>);
impl<T: Clone> ConnIdStore<T> {
    fn new() -> ConnIdStore<T> {
        ConnIdStore(RwLock::new(Slab::with_key()))
    }

    /// Create a new ConnId that links to the item
    fn create_link(&self, item: T) -> ConnId {
        self.0.write().unwrap().insert(item)
    }

    /// Remove all links for which predicate(item) is `true`
    fn remove_associated(&self, predicate: impl Fn(&T) -> bool) {
        self.0.write().unwrap().retain(|_, item| !predicate(item))
    }

    fn follow(&self, conn_id: ConnId) -> Option<T> {
        self.0.read().unwrap().get(conn_id).map(Clone::clone)
    }
}

pub struct ServerState {
    rooms:      CHashMap<RoomId, GameRoom>,
    conn_links: ConnIdStore<(Id, RoomId)>,
}

pub enum ExpectFailure {
    Refuse,
    NotFound,
}

impl ServerState {
    pub fn new() -> ServerState {
        ServerState {
            rooms:      CHashMap::new(),
            conn_links: ConnIdStore::new(),
        }
    }

    /// Create a new game room and returns its Id.
    /// panics if there are more than usize::MAX / 2 rooms on the server
    pub fn create_room(self: Arc<Self>) -> RoomId {
        let (roomid, roomid_copy) = loop {
            let roomid = RoomId::new_random();
            if !self.rooms.contains_key(&roomid) {
                break (roomid.clone(), roomid);
            } else if self.rooms.len() >= std::usize::MAX / 2 {
                panic!("Too many rooms present on server")
            };
        };
        let self_ref = self.clone();
        let on_empty = move || {
            info!("Removing {} from server", &roomid_copy);
            self_ref.remove(&roomid_copy);
        };
        info!("Creating {} on server", &roomid);
        let game = GameRoom::new(roomid.to_string(), on_empty);
        self.rooms.insert(roomid.clone(), game);
        roomid
    }

    /// Remove all references to a room from the server
    pub fn remove(&self, roomid: &RoomId) -> Option<GameRoom> {
        self.conn_links
            .remove_associated(|(_, links_to)| links_to == roomid);
        self.rooms.remove(roomid)
    }

    /// Try to insert new user into given room. Returns the associated `ConnId`
    /// if the given room exists and accepts the newcomer.
    pub fn expect(
        &self,
        roomid: RoomId,
        username: Name,
    ) -> Result<ConnId, ExpectFailure> {
        use self::ManagerResponse::{Accept, Refuse};
        let game_response = self
            .rooms
            .get_mut(&roomid)
            .map(move |mut room| room.expect(username));
        match game_response {
            Some(Accept(id)) => Ok(self.conn_links.create_link((id, roomid))),
            Some(Refuse) => Err(ExpectFailure::Refuse),
            None => Err(ExpectFailure::NotFound),
        }
    }

    pub fn connect(
        &self,
        conn: ConnId,
        ws: Ws2,
    ) -> Result<impl Reply, ConnError> {
        let (player, roomid) =
            self.conn_links.follow(conn).ok_or(ConnError::NotTracked)?;
        let mut room =
            self.rooms.get_mut(&roomid).ok_or(ConnError::NotFound)?;
        Ok(room.accept(player, ws))
    }

    pub fn is_active(&self, roomid: &RoomId) -> bool {
        self.rooms.contains_key(roomid)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_connids() {
        let store = ConnIdStore::new();

        let conn_id1 = store.create_link(1111);
        assert_eq!(store.follow(conn_id1), Some(1111));

        let conn_id2 = store.create_link(2222);
        assert_eq!(store.follow(conn_id2), Some(2222));

        let conn_id3 = store.create_link(3333);
        assert_eq!(store.follow(conn_id3), Some(3333));

        assert_eq!(store.follow(conn_id2), Some(2222));

        store.remove_associated(|&x| x < 3000);
        assert_eq!(store.follow(conn_id1), None);
        assert_eq!(store.follow(conn_id2), None);
        assert_eq!(store.follow(conn_id3), Some(3333));

        let conn_id4 = store.create_link(1111);
        assert_eq!(store.follow(conn_id4), Some(1111));
        assert_eq!(store.follow(conn_id1), None);
    }

    #[test]
    fn test_serlialize_connids() {
        let store = ConnIdStore::new();
        let conn_id1 = store.create_link(1111);

        let conn_id1_round_trip =
            ConnId::from_str(&conn_id1.into_url()).unwrap();

        assert_eq!(store.follow(conn_id1_round_trip), Some(1111));
        let conn_id2 = store.create_link(2222);

        let conn_id2_round_trip =
            ConnId::from_str(&conn_id2.into_url()).unwrap();

        assert_eq!(store.follow(conn_id2_round_trip), Some(2222));
        assert_eq!(store.follow(conn_id1_round_trip), Some(1111));
    }
}
