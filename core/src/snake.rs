use emath::{pos2, vec2, Pos2, Vec2};
use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;

use crate::interaction::Interaction;
use crate::style::ColScheme;

/// Snake control functionality
#[derive(Debug, Clone)]
pub struct SnakePilot {
    state: SnakeState,
    snake: Snake,
}
impl SnakePilot {
    pub fn new(snake: Snake) -> Self {
        Self {
            state: SnakeState::Anchored(snake.data.npos(0)),
            snake,
        }
    }
    pub fn respawn(&mut self, position: Pos2, order: usize) {
        self.snake.data.derivatives = vec![vec2(0., 0.); order + 1];
        self.snake.data.derivatives[0] = position.to_vec2();
        self.snake.data.order = order;
        self.anchor();
    }
    pub fn snake(&self) -> &Snake {
        &self.snake
    }
    pub(crate) fn snake_mut(&mut self) -> &mut Snake {
        &mut self.snake
    }
    pub fn state(&self) -> &SnakeState {
        &self.state
    }
    pub fn state_mut(&mut self) -> &mut SnakeState {
        &mut self.state
    }
    pub fn follow(&mut self, target: Pos2) {
        self.state = SnakeState::Following(target);
        let snake_data = &mut self.snake.data;
        *snake_data.derivatives.last_mut().unwrap() = target
            - if snake_data.order > 0 {
                snake_data.npos(snake_data.order - 1)
            } else {
                pos2(0., 0.)
            };
    }
    pub fn anchor(&mut self) {
        self.state = SnakeState::Anchored(self.snake.data.npos(self.snake.data.order));
    }
    pub(crate) fn add(&mut self) {
        self.snake.add();
        match self.state {
            SnakeState::Anchored(anchor) => {
                let mut rng = rand::thread_rng();
                let theta = rng.gen::<f32>() * std::f32::consts::PI * 2.;
                self.state = SnakeState::Anchored(anchor + 0.01 * vec2(theta.cos(), theta.sin()));
            }
            _ => {}
        }
    }
    pub(crate) fn remove(&mut self) {
        self.snake.remove();
    }
    pub fn interact(&mut self, other: &Snake, dt: f32) {
        if let Some(interaction) = self.snake.interaction {
            interaction.interact(other.data.derivatives[0].to_pos2(), self, dt);
        }
    }
}

/// Snake trail functionality
#[derive(Debug, Clone)]
pub struct SnakeHistory {
    history: VecDeque<Vec<Pos2>>,
    memory: usize,
    leading_trail: bool,
}
impl SnakeHistory {
    fn new() -> Self {
        Self {
            memory: 100, //reset to 200
            history: VecDeque::new(),
            leading_trail: false,
        }
    }
    pub fn history(&self) -> &VecDeque<Vec<Pos2>> {
        &self.history
    }
    pub fn memory(&self) -> usize {
        self.memory
    }
    pub fn toggle_leading_trail(&mut self) {
        self.leading_trail = !self.leading_trail;
    }
    pub fn leading_trail(&self) -> bool {
        self.leading_trail
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SnakeData {
    id: u8,
    team: u8,
    order: usize,
    derivatives: Vec<Vec2>,
    scheme: ColScheme,
}
impl SnakeData {
    fn new(id: u8, order: usize) -> Self {
        Self {
            id,
            team: 0,
            order,
            derivatives: vec![vec2(0., 0.); order + 1],
            scheme: ColScheme::Sinebow,
        }
    }
    pub fn id(&self) -> u8 {
        self.id
    }
    pub fn derivatives(&self) -> &Vec<Vec2> {
        &self.derivatives
    }
    pub fn add(&mut self) {
        self.order += 1;
        self.derivatives.push(vec2(0., 0.));
    }
    pub fn remove(&mut self) {
        if self.order > 0 {
            self.order -= 1;
            self.derivatives.pop();
        }
    }
    pub fn set_order(&mut self, order: usize) {
        while self.order > order {
            self.remove();
        }
        while self.order < order {
            self.add();
        }
    }
    pub fn order(&self) -> usize {
        self.order
    }
    pub fn scheme(&self) -> ColScheme {
        self.scheme
    }
    pub fn set_scheme(&mut self, scheme: ColScheme) {
        self.scheme = scheme
    }
    pub fn cycle_scheme(&mut self) {
        self.scheme = self.scheme.next_scheme()
    }

    pub fn npos(&self, n: usize) -> Pos2 {
        self.derivatives
            .iter()
            .take(n + 1)
            .fold(pos2(0., 0.), |a, &b| a + b)
    }
}

#[derive(Debug, Clone)]
pub struct Snake {
    data: SnakeData,
    history: SnakeHistory,
    interaction: Option<Interaction>,
}
impl Snake {
    pub fn new(id: u8, order: usize) -> Self {
        Self {
            data: SnakeData::new(id, order),
            history: SnakeHistory::new(),
            interaction: None,
        }
    }
    pub fn data(&self) -> &SnakeData {
        &self.data
    }
    pub fn set_data(&mut self, data: SnakeData) {
        self.data = data;
    }
    pub fn id(&self) -> u8 {
        self.data.id
    }
    pub fn set_id(&mut self, id: u8) {
        self.data.id = id
    }
    pub fn team(&self) -> u8 {
        self.data.team
    }
    pub fn set_team(&mut self, team: u8) {
        self.data.team = team
    }
    pub fn derivatives_mut(&mut self) -> &mut Vec<Vec2> {
        &mut self.data.derivatives
    }
    fn add(&mut self) {
        self.data.add();
    }
    fn remove(&mut self) {
        self.data.remove();
    }
    pub fn history(&self) -> &SnakeHistory {
        &self.history
    }
    pub(crate) fn step_history(&mut self) {
        self.history.history.push_back(
            (0..(self.data.order + if self.history.leading_trail { 1 } else { 0 }))
                .map(|i| self.data.npos(i))
                .collect(),
        );
        while self.history.history.len() > self.history.memory {
            self.history.history.pop_front();
        }
    }
    pub fn toggle_leading_trail(&mut self) {
        self.history.toggle_leading_trail();
    }
    pub fn set_scheme(&mut self, scheme: ColScheme) {
        self.data.set_scheme(scheme);
    }
    pub fn cycle_scheme(&mut self) {
        self.data.cycle_scheme();
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SnakeState {
    Following(Pos2),
    Linked(LinkType),
    Anchored(Pos2),
    Drifting,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum LinkType {
    ToSelf(usize),
    ToOther(u8, usize),
}
