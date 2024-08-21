use serde::{Deserialize, Serialize};

use crate::{snake::SnakeData, SnakeTeam};

/// Client-server messages
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Message {
    Connect,
    Id(u8),
    Snake(SnakeData),
    Disconnect,
    Heartbeat,
    Lobby(String),
    Join(u8),
    Leave(u8),
    RegisterTeam(SnakeTeam),
    StartArena,
    EndArena(SnakeTeam),
}
impl Message {
    pub fn ser(&self) -> Vec<u8> {
        serde_json::to_string(self).unwrap().as_bytes().to_vec()
    }
    pub fn deser(b: &[u8]) -> Result<Self, serde_json::Error> {
        serde_json::from_slice(b)
    }
}
