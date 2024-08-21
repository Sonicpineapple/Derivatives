use std::{collections::HashMap, fmt::Debug};

use emath::{pos2, Pos2};

use crate::{interaction::Interaction, snake::SnakePilot, style::ColSingle, SnakeTeam, Value};

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
pub struct SpawnPoint {
    position: Pos2,
    order: Value<usize>,
}
impl SpawnPoint {
    pub fn new(position: Pos2, order: impl Into<Value<usize>>) -> Self {
        Self {
            position,
            order: order.into(),
        }
    }
    pub fn default() -> Self {
        Self {
            position: pos2(0., 0.),
            order: Value::Const(2),
        }
    }
    pub fn position(&self) -> Pos2 {
        self.position
    }
    pub fn order(&self) -> Value<usize> {
        self.order
    }
}

#[derive(Debug, Clone)]
pub enum Spawner {
    Standard {
        spawn_point: SpawnPoint,
    },
    Team {
        spawners: HashMap<SnakeTeam, SpawnPoint>,
    },
}
impl Spawner {
    pub fn new(spawn_point: SpawnPoint) -> Self {
        Self::Standard { spawn_point }
    }
    pub fn new_team(points: Vec<(SnakeTeam, SpawnPoint)>) -> Self {
        let mut spawners = HashMap::new();
        for (team, spawn_point) in points {
            spawners.insert(team, spawn_point);
        }
        Self::Team { spawners }
    }
    pub fn default() -> Self {
        Self::Standard {
            spawn_point: SpawnPoint::default(),
        }
    }
    pub fn spawner_for(&self, snake: &SnakePilot) -> &SpawnPoint {
        match self {
            Spawner::Standard { spawn_point } => spawn_point,
            Spawner::Team { spawners } => spawners
                .get(&snake.snake().team().expect("No team to spawn"))
                .expect("No such spawner"),
        }
    }
}
impl From<SpawnPoint> for Spawner {
    fn from(value: SpawnPoint) -> Self {
        Self::new(value)
    }
}

#[derive(Debug, Clone)]
pub struct Text {
    position: Pos2,
    size: f32,
    text: Value<&'static str>,
}
impl Text {
    pub fn new(position: Pos2, size: f32, text: Value<&'static str>) -> Self {
        Self {
            position,
            size,
            text,
        }
    }
    pub fn order() -> Self {
        Self::new(pos2(0., 0.5), 2. / 7., Value::SnakeOrder)
    }
    pub fn score() -> Self {
        Self::new(pos2(0., 0.), 1. / 2., Value::Score)
    }
    pub fn last_winner() -> Self {
        Self::new(pos2(0., -0.5), 2. / 7., Value::LastWinner)
    }
    pub fn arena_order() -> Self {
        Self::new(pos2(0., 0.), 2. / 7., Value::ArenaOrder)
    }
    pub fn position(&self) -> Pos2 {
        self.position
    }
    pub fn size(&self) -> f32 {
        self.size
    }
    pub fn text(&self) -> Value<&str> {
        self.text
    }
}
