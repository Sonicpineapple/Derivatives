use serde::{Deserialize, Serialize};

use crate::snake::SnakeData;

/// Client-server messages
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Message {
    Connect,
    Id(u8),
    Snake(SnakeData),
    Disconnect,
    Heartbeat,
    Join(u8),
    Leave(u8),
    RegisterTeam(u8),
    StartArena,
    EndArena(u8),
}
impl Message {
    pub fn ser(&self) -> Vec<u8> {
        serde_json::to_string(self).unwrap().as_bytes().to_vec()
    }
    pub fn deser(b: &[u8]) -> Result<Self, serde_json::Error> {
        serde_json::from_slice(b)
    }
}
