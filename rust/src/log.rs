use serde::Deserialize;

#[derive(Debug, PartialEq, Eq, Deserialize)]
pub struct Log {
    pub address: String,
    pub data: String,
    pub topics: Vec<String>,
}
