use crate::{ColScheme, ScreenId, SnakeTeam, WorldType};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum GameAction {
    Respawn,
    Reset,
    Move(ScreenId),
    World(WorldType),
    Exit,
    Point,
    GenerateGoal,
    JoinMultiplayer,
    LeaveMultiplayer,
    OpenLobbySelect,
    RegisterTeam(SnakeTeam),
    SetColScheme(ColScheme),
    CycleColScheme,
    ToggleLeadingTrail,
    AdjustNodeCount(isize),
    AdjustArenaOrder(isize),
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NetworkAction {
    RegisterTeam(SnakeTeam),
    JoinMultiplayer,
    LeaveMultiplayer,
    AdjustArenaOrder(isize),
}
