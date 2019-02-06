//! Dynamic and static pages included in the REST spec
use bart_derive::BartDisplay;
use std::fmt;

#[derive(BartDisplay)]
#[template = "pages-server/join-star-uid.html"]
pub struct JoinFormBody(pub super::RoomId);

pub struct JoinFormErr;
impl fmt::Display for JoinFormErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, include_str!("../../pages-server/join-star-uid-err.html"))
    }
}

const JOIN_REDIRECT_ERR: &str =
    include_str!("../../pages-server/join-roomid-err.html");
pub struct JoinRedirectErr;
impl Into<&'static str> for JoinRedirectErr {
    fn into(self) -> &'static str {
        JOIN_REDIRECT_ERR
    }
}
