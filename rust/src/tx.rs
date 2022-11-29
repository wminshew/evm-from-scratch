use ethereum_types::U256;
use serde::Deserialize;

#[derive(Debug, Default, Clone, Deserialize)]
pub struct Tx {
    #[serde(default)]
    pub to: U256,
    #[serde(default)]
    pub from: U256,
    #[serde(default)]
    pub origin: U256,
    #[serde(default)]
    pub gasprice: U256,
    #[serde(default)]
    pub value: U256,
    #[serde(default)]
    pub data: String,
}
