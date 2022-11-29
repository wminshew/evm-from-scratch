// use ethereum_types::U256;
// use serde::de::Visitor;
// use serde::{de, Deserializer};
// use std::fmt;

// pub fn u256_to_address(u: &U256) -> String {
//     format!("0x{:0>40x}", u)
// }

// pub fn u256_to_string(u: &U256) -> String {
//     format!("0x{:x}", u)
// }

// pub fn str_to_u256(s: &str) -> U256 {
//     U256::from_str_radix(s, 16).expect("invalid U256")
// }

// pub fn deserialize_u256<'de, D>(deserializer: D) -> Result<U256, D::Error>
// where
//     D: Deserializer<'de>,
// {
//     struct U256Visitor;

//     impl<'de> Visitor<'de> for U256Visitor {
//         type Value = U256;

//         fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
//             formatter.write_str("struct U256")
//         }

//         fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
//         where
//             E: de::Error,
//         {
//             U256::from_str_radix(s, 16).map_err(de::Error::custom)
//         }
//     }

//     deserializer.deserialize_str(U256Visitor)
// }
