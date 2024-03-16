use std::{collections::HashMap, fmt::Debug};

use emath::{pos2, Pos2};

use crate::{
    interaction::Interaction,
    snake::SnakePilot,
    style::{ColScheme, ColSingle},
    world::{ScreenId, ScreenIndex, WorldType},
};

#[derive(Debug, Clone)]
pub struct Hazard {
    centre: Pos2,
    radius: f32,
    col: ColSingle,
    interaction: Interaction,
}
impl Hazard {
    pub(crate) fn new_attractor(centre: Pos2, radius: f32, strength: f32) -> Self {
        Self {
            centre,
            radius,
            col: ColSingle::Black,
            interaction: Interaction::Attract(strength),
        }
    }

    pub(crate) fn interact(&self, snake: &mut SnakePilot, dt: f32) {
        self.interaction.interact(self.centre, snake, dt);
    }

    pub fn centre(&self) -> Pos2 {
        self.centre
    }
    pub fn radius(&self) -> f32 {
        self.radius
    }
    pub fn col(&self) -> ColSingle {
        self.col
    }
}

#[derive(Debug, Clone)]
pub enum Spawner {
    Standard { position: Pos2, order: usize },
    Team { spawners: HashMap<u8, Spawner> },
}
impl Spawner {
    pub fn new(position: Pos2, order: usize) -> Self {
        Self::Standard { position, order }
    }
    pub fn new_team(points: Vec<(u8, Pos2, usize)>) -> Self {
        let mut spawners = HashMap::new();
        for (id, position, order) in points {
            spawners.insert(id, Self::Standard { position, order });
        }
        Self::Team { spawners }
    }
    pub fn default() -> Self {
        Self::Standard {
            position: pos2(0., 0.),
            order: 2,
        }
    }
    pub fn respawn(&self, snake: &mut SnakePilot) {
        match self {
            Spawner::Standard { position, order } => snake.respawn(*position, *order),
            Spawner::Team { spawners } => {
                let point = spawners.get(&snake.snake().team()).expect("No spawner");
                point.respawn(snake);
            }
        }
    }
}

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
    RegisterTeam(u8),
    SetColScheme(ColScheme),
    CycleColScheme,
    ToggleLeadingTrail,
    AdjustNodeCount(isize),
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NetworkAction {
    RegisterTeam(u8),
    JoinMultiplayer,
    LeaveMultiplayer,
}

#[derive(Debug, Clone)]
pub struct Text {
    position: Pos2,
    size: f32,
    text: TextType,
}
impl Text {
    pub fn new(position: Pos2, size: f32, text: TextType) -> Self {
        Self {
            position,
            size,
            text,
        }
    }
    pub fn order() -> Self {
        Self::new(pos2(0., 0.5), 2. / 7., TextType::SnakeOrder)
    }
    pub fn score() -> Self {
        Self::new(pos2(0., 0.), 1. / 2., TextType::Score)
    }
    pub fn last_winner() -> Self {
        Self::new(pos2(0., -0.5), 2. / 7., TextType::LastWinner)
    }
    pub fn position(&self) -> Pos2 {
        self.position
    }
    pub fn size(&self) -> f32 {
        self.size
    }
    pub fn text(&self) -> TextType {
        self.text
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TextType {
    SnakeOrder,
    Score,
    LastWinner,
    Text(&'static str),
}
