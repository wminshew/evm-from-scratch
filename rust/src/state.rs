use crate::code::Code;
use ethereum_types::U256;
use serde::Deserialize;
use std::collections::HashMap;
use std::ops::Add;

// TODO: use address instead of U256?

#[derive(Debug, Default, Deserialize)]
pub struct State(HashMap<U256, StateInfo>);

#[derive(Debug, Default, Clone, Deserialize)]
pub struct StateInfo {
    #[serde(default)]
    balance: U256,
    #[serde(default)]
    nonce: U256,
    #[serde(default)]
    code: Code,
    #[serde(default)]
    storage: Storage,
}

pub type Storage = HashMap<U256, U256>;

impl State {
    pub fn get_balance(&self, address: &U256) -> U256 {
        self.0
            .get(address)
            .map(|addr| addr.balance)
            .unwrap_or_else(U256::zero)
    }

    pub fn get_nonce(&self, address: &U256) -> U256 {
        self.0
            .get(address)
            .map(|addr| addr.nonce)
            .unwrap_or_else(U256::zero)
    }

    pub fn get_code(&self, address: &U256) -> Vec<u8> {
        hex::decode(
            self.0
                .get(address)
                .map(|si| si.code.bin.to_string())
                .unwrap_or_else(|| "".to_string()),
        )
        .unwrap()
    }

    pub fn load(&self, address: &U256, key: &U256) -> U256 {
        self.0
            .get(address)
            .map(|si| &si.storage)
            .and_then(|s| s.get(key))
            .map(|k| k.to_owned())
            .unwrap_or_else(U256::zero)
    }

    pub fn store(&mut self, address: &U256, key: &U256, value: &U256) {
        self.0
            .entry(address.to_owned())
            .or_default()
            .storage
            .insert(key.to_owned(), value.to_owned());
    }

    pub fn deploy(&mut self, address: &U256, value: &U256, code: &str) {
        self.0.insert(
            address.to_owned(),
            StateInfo {
                balance: value.to_owned(),
                code: Code {
                    bin: code.to_string(),
                    ..Code::default()
                },
                ..StateInfo::default()
            },
        );
    }

    pub fn self_destruct(&mut self, address: &U256, send_balance_to: &U256) {
        let my_balance = self.get_balance(address);

        let mut e = self.0.entry(send_balance_to.to_owned()).or_default();
        e.balance = e.balance.add(my_balance);

        let nonce = e.nonce.to_owned();
        self.0.insert(
            address.to_owned(),
            StateInfo {
                nonce,
                ..StateInfo::default()
            },
        );
    }
}
