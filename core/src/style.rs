use serde::{Deserialize, Serialize};

use crate::SnakeTeam;

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq)]
pub enum ColScheme {
    Sinebow,
    Reds,
    Greens,
    Blues,
    Purples,
    Grays,
    Spectral,
    Cool,
    Warm,
}
impl ColScheme {
    pub(crate) fn next_scheme(&self) -> Self {
        match self {
            ColScheme::Sinebow => ColScheme::Reds,
            ColScheme::Reds => ColScheme::Greens,
            ColScheme::Greens => ColScheme::Blues,
            ColScheme::Blues => ColScheme::Purples,
            ColScheme::Purples => ColScheme::Grays,
            ColScheme::Grays => ColScheme::Spectral,
            ColScheme::Spectral => ColScheme::Cool,
            ColScheme::Cool => ColScheme::Warm,
            ColScheme::Warm => ColScheme::Sinebow,
        }
    }
}
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ColSingle {
    LightRed,
    LightGreen,
    LightBlue,
    DarkGrey,
    DarkRed,
    DarkBlue,
    Gold,
    Black,
}

pub fn get_team_col(team: SnakeTeam) -> ColSingle {
    match team {
        SnakeTeam::Spectator => ColSingle::DarkGrey,
        SnakeTeam::Team(team) => match team {
            1 => ColSingle::DarkRed,
            2 => ColSingle::DarkBlue,
            _ => ColSingle::DarkGrey,
        },
    }
}
