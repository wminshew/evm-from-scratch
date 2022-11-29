use ethereum_types::U256;
use serde::Deserialize;

#[derive(Debug, Default, Deserialize)]
pub struct Block {
    #[serde(default)]
    pub basefee: U256,
    #[serde(default)]
    pub coinbase: U256,
    #[serde(default)]
    pub timestamp: U256,
    #[serde(default)]
    pub number: U256,
    #[serde(default)]
    pub difficulty: U256,
    #[serde(default)]
    pub gaslimit: U256,
    #[serde(default)]
    pub chainid: U256,
}
