#[macro_export]
macro_rules! impl_deserialize_with_from_str {
    ($type:ty) => {
        impl<'de> Deserialize<'de> for $type {
            fn deserialize<D>(deserialize: D) -> Result<$type, D::Error>
            where
                D: Deserializer<'de>,
            {
                let d = <&str>::deserialize(deserialize)?;
                <$type>::from_str(d).map_err(de::Error::custom)
            }
        }
    };
}
