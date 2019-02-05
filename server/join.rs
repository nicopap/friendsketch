//! Create unique pages for one-use access to game rooms

use quick_error::quick_error;
use std::str::FromStr;

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct Uid([u8; 4]);
impl Uid {
    pub fn new<R: rand::Rng + ?Sized>(rng: &mut R) -> Uid {
        Uid(rng.gen())
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum UidError {
        BadSize {
            display("A join uid is a base64-encoded 4 bytes value")
        }
        BadBase64(err: base64::DecodeError) {
            from()
        }
    }
}

/// Return the array inside Some(_), or None if there were too few elements
/// <https://stackoverflow.com/questions/49328371/how-to-convert-a-vec-into-an-array-without-copying-the-elements>
fn take_array4<T>(v: Vec<T>) -> Option<[T; 4]> {
    let mut iter = v.into_iter();
    if let (Some(x), Some(y), Some(z), Some(w)) =
        (iter.next(), iter.next(), iter.next(), iter.next())
    {
        Some([x, y, z, w])
    } else {
        None
    }
}

impl FromStr for Uid {
    type Err = UidError;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        let inner = base64::decode_config(text, base64::URL_SAFE_NO_PAD)?;
        if let Some(inner) = take_array4(inner) {
            Ok(Uid(inner))
        } else {
            Err(UidError::BadSize)
        }
    }
}

impl Into<String> for &Uid {
    fn into(self) -> String {
        base64::encode_config(&self.0, base64::URL_SAFE_NO_PAD)
    }
}
