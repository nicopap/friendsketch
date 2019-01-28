mod adjective;
mod animal;

use rand::{thread_rng, Rng};

use self::{adjective::LIST as ADJECTIVES, animal::LIST as ANIMALS};

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash, Clone)]
pub(in crate::api) struct RoomId {
    adjective: u8,
    animal:    u8,
}
impl RoomId {
    pub(in crate::api) fn try_from(name: &str) -> Option<Self> {
        let dot_index = name.find('.')?;
        let (adj_val, ani_val) = name.split_at(dot_index + 1);
        let adjective = adjective::from(adj_val)?;
        let animal = animal::from(ani_val)?;
        Some(RoomId { adjective, animal })
    }
}

impl From<u16> for RoomId {
    fn from(index: u16) -> RoomId {
        let adjective = ((index & 0xff00) >> 8) as u8;
        let animal = (index & 0x00ff) as u8;
        RoomId { adjective, animal }
    }
}
impl From<&RoomId> for u16 {
    fn from(&RoomId { adjective, animal }: &RoomId) -> u16 {
        let ret = animal as u16;
        ret | ((adjective as u16) << 8)
    }
}
impl From<RoomId> for u16 {
    fn from(RoomId { adjective, animal }: RoomId) -> u16 {
        let ret = animal as u16;
        ret | ((adjective as u16) << 8)
    }
}
impl From<&RoomId> for String {
    fn from(&RoomId { adjective, animal }: &RoomId) -> String {
        let mut ret = ADJECTIVES[adjective as usize].to_owned();
        ret.push_str(ANIMALS[animal as usize]);
        ret
    }
}

pub(in crate::api) fn gen() -> RoomId {
    let (adjective, animal): (u8, u8) = thread_rng().gen();
    RoomId { adjective, animal }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::{__assert_ne, assert_eq, assert_ne};
    use rand::random;

    #[test]
    fn test_bijective() {
        for _ in 1..100000 {
            let val: u16 = random();
            let room_val: String = (&RoomId::from(val)).into();
            println!("{}", &room_val);
            let val_after: u16 = RoomId::try_from(&room_val).unwrap().into();
            assert_eq!(val, val_after);
        }
    }
    #[test]
    fn test_no_injective() {
        for _ in 1..100000 {
            let val: u16 = random();
            let val2: u16 = random();
            let room_val: String = (&RoomId::from(val)).into();
            let room_val2: String = (&RoomId::from(val2)).into();
            if val == val2 {
                assert_eq!(room_val, room_val2);
            } else {
                assert_ne!(room_val, room_val2);
            }
        }
    }
}
