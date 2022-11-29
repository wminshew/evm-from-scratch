use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Memory(Vec<u8>);

impl Memory {
    pub fn new() -> Self {
        Memory(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.len() == 0
    }

    pub fn store(&mut self, offset: usize, data: &[u8]) {
        let end = offset + data.len();
        self.resize(end);
        self.0[offset..end].copy_from_slice(data);
    }

    pub fn load(&mut self, offset: usize, size: usize) -> &[u8] {
        let end = offset + size;
        self.resize(end);
        &self.0[offset..end]
    }

    fn resize(&mut self, end: usize) {
        if end > self.0.len() {
            self.0.resize(((end + 31) / 32) * 32, 0);
        }
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self::new()
    }
}
