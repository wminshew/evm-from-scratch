use serde::Deserialize;

#[derive(Debug, Default, Clone, Deserialize)]
pub struct Code {
    #[serde(default)]
    pub asm: Option<String>,
    #[serde(default)]
    pub bin: String,
}
