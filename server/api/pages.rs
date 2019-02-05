//! Dynamic and static pages included in the REST spec
use bart_derive::BartDisplay;
use std::fmt;

#[derive(BartDisplay)]
#[template = "pages-server/join-star-uid.html"]
pub struct JoinRoomBody(pub super::RoomId);

pub struct JoinRoomErr;
impl fmt::Display for JoinRoomErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, include_str!("../../pages-server/join-star-uid-err.html"))
    }
}

const JOIN_FORM_ERR: &str =
    include_str!("../../pages-server/join-roomid-err.html");
pub struct JoinFormErr;
impl Into<&'static str> for JoinFormErr {
    fn into(self) -> &'static str {
        JOIN_FORM_ERR
    }
}
