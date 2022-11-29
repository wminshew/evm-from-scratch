use ethereum_types::U256;
use std::cmp;
use std::ops::{Deref, DerefMut};

const JUMPDEST: u8 = 0x5b;

#[derive(Debug)]
pub struct PC<'a> {
    counter: usize,
    code: &'a [u8],
}

impl<'a> PC<'a> {
    pub fn new(code: &'a [u8]) -> Self {
        PC { counter: 0, code }
    }

    pub fn current(&self) -> U256 {
        // subtract 1 to correct for iter auto-inc
        U256::from(self.counter - 1)
    }

    pub fn push(&mut self, size: usize) -> U256 {
        let data = U256::from(&self.code[self.counter..self.counter + size]);
        self.counter += size;
        data
    }

    pub fn jump(&mut self, loc: usize) -> bool {
        self.counter = loc;
        // in-bounds
        !(self.counter >= self.code.len()
            // at a JUMPDEST
                || self.opcode() != JUMPDEST
            // non-push-data
                || (1..=cmp::min(16, self.counter))
                .any(|i|
                     self.code[self.counter - i] >= 0x60 + i as u8 - 1
                     && self.code[self.counter - i] < 0x80
                ))
    }

    fn opcode(&self) -> u8 {
        self.code[self.counter]
    }
}

impl Deref for PC<'_> {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.counter
    }
}

impl DerefMut for PC<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.counter
    }
}

impl Iterator for PC<'_> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.counter < self.code.len() {
            let opcode = self.opcode();
            self.counter += 1;
            Some(opcode)
        } else {
            None
        }
    }
}
