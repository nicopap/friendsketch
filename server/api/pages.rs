//! Dynamic and static pages included in the REST spec
use bart_derive::BartDisplay;

#[derive(BartDisplay)]
#[template = "pages-server/join-star-uid.html"]
pub struct JoinRoomBody(pub super::RoomId);
impl Into<String> for JoinRoomBody {
    fn into(self) -> String {
        format!("{}", self)
    }
}

const JOIN_ROOM_ERR: &str =
    include_str!("../../pages-server/join-star-uid-err.html");
pub struct JoinRoomErr;
impl Into<&'static str> for JoinRoomErr {
    fn into(self) -> &'static str {
        JOIN_ROOM_ERR
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
