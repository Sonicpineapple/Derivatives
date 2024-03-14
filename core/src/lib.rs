use emath::{pos2, vec2, Pos2, Vec2};
use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, VecDeque},
    f32::consts::PI,
};

/// Get position from radius and cw angle with 0 radians as -y
fn pos_rt(r: f32, t: f32) -> Pos2 {
    pos2(0., 0.) + vec_rt(r, t)
}
/// Get vector from radius and cw angle with 0 radians as -y
fn vec_rt(r: f32, t: f32) -> Vec2 {
    r * vec2(t.sin(), -t.cos())
}

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
    fn next_scheme(&self) -> Self {
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
    Gold,
    Black,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Snake {
    id: u8,
    team: u8,
    order: usize,
    derivatives: Vec<Vec2>,
    state: SnakeState,
    memory: usize,
    history: VecDeque<Vec<Pos2>>,
    scheme: ColScheme,
    leading_trail: bool,
}
impl Snake {
    pub fn new(id: u8, order: usize) -> Self {
        Self {
            id,
            team: 0,
            order,
            derivatives: vec![vec2(0., 0.); order + 1],
            state: SnakeState::Anchored(pos2(0., 0.)),
            memory: 100, //reset to 200
            history: VecDeque::new(),
            scheme: ColScheme::Sinebow,
            leading_trail: false,
        }
    }
    // fn draw(&self, ui: &mut egui::Ui, trans: &dyn Fn(Pos2) -> Pos2, unit: f32) {
    //     let node_rad = unit / 50.;
    //     let line_width = unit / 80.;
    //     for (t, h) in self.history.iter().enumerate() {
    //         for (i, &e) in h.iter().enumerate() {
    //             if self.leading_trail || i < self.order {
    //                 let col = self
    //                     .spectrum
    //                     .gradient()
    //                     .eval_rational(i, h.len() + if self.leading_trail { 0 } else { 1 });
    //                 let col = egui::Color32::from_rgba_unmultiplied(
    //                     col.r,
    //                     col.g,
    //                     col.b,
    //                     (t * 255 / (4 * self.memory)) as u8,
    //                 );
    //                 ui.painter().circle_filled(
    //                     trans(e),
    //                     t as f32 * node_rad / (3 * self.memory) as f32,
    //                     col,
    //                 );
    //             }
    //         }
    //     }
    //     for i in 1..self.derivatives.len() {
    //         ui.painter().line_segment(
    //             [trans(self.npos(i - 1)), trans(self.npos(i))],
    //             (line_width, egui::Color32::DARK_GRAY),
    //         )
    //     }
    //     for i in 0..self.derivatives.len() {
    //         let col = colorous::SINEBOW.eval_rational(i, self.order + 1);
    //         let col = egui::Color32::from_rgb(col.r, col.g, col.b);
    //         ui.painter()
    //             .circle_filled(trans(self.npos(i)), node_rad, col);
    //     }
    // }
    fn step(&mut self, dt: f32, friction: f32) {
        self.step_history();
        for i in (1..(self.derivatives.len())).rev() {
            let temp = self.derivatives[i];
            self.derivatives[i - 1] += temp * dt;
        }
        for i in &mut self.derivatives[1..] {
            *i *= 1. - friction;
        }
        match self.state {
            SnakeState::Following(target) => {
                *self.derivatives.last_mut().unwrap() = target
                    - if self.order > 0 {
                        self.npos(self.order - 1)
                    } else {
                        pos2(0., 0.)
                    };
            }
            SnakeState::Linked(index) => {
                *self.derivatives.last_mut().unwrap() = self.npos(index)
                    - if self.order > 0 {
                        self.npos(self.order - 1)
                    } else {
                        self.npos(0)
                    };
            }
            SnakeState::Anchored(anchor) => {
                *self.derivatives.last_mut().unwrap() = anchor
                    - if self.order > 0 {
                        self.npos(self.order - 1)
                    } else {
                        pos2(0., 0.)
                    };
            }
            SnakeState::Drifting => todo!(),
        }
    }
    fn step_history(&mut self) {
        self.history.push_back(
            (0..(self.order + if self.leading_trail { 1 } else { 0 }))
                .map(|i| self.npos(i))
                .collect(),
        );
        while self.history.len() > self.memory {
            self.history.pop_front();
        }
    }
    pub fn history(&self) -> &VecDeque<Vec<Pos2>> {
        &self.history
    }
    pub fn memory(&self) -> usize {
        self.memory
    }
    pub fn derivatives(&self) -> &Vec<Vec2> {
        &self.derivatives
    }

    pub fn add(&mut self) {
        self.order += 1;
        self.derivatives.push(vec2(0., 0.));
        match self.state {
            SnakeState::Anchored(anchor) => {
                let mut rng = rand::thread_rng();
                let theta = rng.gen::<f32>() * std::f32::consts::PI * 2.;
                self.state = SnakeState::Anchored(anchor + 0.01 * vec2(theta.cos(), theta.sin()));
            }
            _ => {}
        }
    }
    pub fn remove(&mut self) {
        if self.order > 0 {
            self.order -= 1;
            self.derivatives.pop();
        }
    }
    fn set_order(&mut self, order: usize) {
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
    pub fn state_mut(&mut self) -> &mut SnakeState {
        &mut self.state
    }

    pub fn toggle_leading_trail(&mut self) {
        self.leading_trail = !self.leading_trail;
    }
    pub fn leading_trail(&self) -> bool {
        self.leading_trail
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
    pub fn reset(&mut self, pos: Pos2) {
        self.derivatives = vec![vec2(0., 0.); self.order + 1];
        self.history = VecDeque::new();
        self.derivatives[0] = pos.to_vec2();
        self.anchor();
    }

    pub fn id(&self) -> u8 {
        self.id
    }
    pub fn set_id(&mut self, id: u8) {
        self.id = id;
    }
    pub fn team(&self) -> u8 {
        self.team
    }
    pub fn set_team(&mut self, team: u8) {
        self.team = team;
    }

    pub fn follow(&mut self, target: Pos2) {
        self.state = SnakeState::Following(target);
        *self.derivatives.last_mut().unwrap() = target
            - if self.order > 0 {
                self.npos(self.order - 1)
            } else {
                pos2(0., 0.)
            };
    }
    pub fn link(&mut self, target: usize) {
        self.state = SnakeState::Linked(target);
        *self.derivatives.last_mut().unwrap() = self.npos(target)
            - if self.order > 0 {
                self.npos(self.order - 1)
            } else {
                pos2(0., 0.)
            };
    }
    pub fn anchor(&mut self) {
        self.state = SnakeState::Anchored(self.npos(self.order));
    }

    pub fn npos(&self, n: usize) -> Pos2 {
        self.derivatives
            .iter()
            .take(n + 1)
            .fold(pos2(0., 0.), |a, &b| a + b)
    }

    pub fn data(&self) -> SnakeData {
        SnakeData {
            id: self.id,
            team: self.team,
            order: self.order,
            derivatives: self.derivatives.clone(),
            state: self.state,
            spectrum: self.scheme,
            leading_trail: self.leading_trail,
        }
    }
    pub fn set_data(&mut self, data: SnakeData) {
        self.set_id(data.id);
        self.set_team(data.team);
        self.leading_trail = data.leading_trail;
        self.step_history();
        self.set_order(data.order);
        self.derivatives = data.derivatives;
        self.state = data.state;
        self.scheme = data.spectrum;
    }
    fn interact(&mut self, other: &Snake, dt: f32) {
        let dir = other.derivatives[0].to_pos2() - self.derivatives[0].to_pos2();
        let dist = (0.001 as f32).max(dir.length_sq());
        let dir = dir.normalized();
        match self.order {
            0 => match self.state {
                SnakeState::Anchored(_) => {
                    self.derivatives[0] += dir * 0.01 * dt * dt / dist;
                    self.anchor()
                }
                SnakeState::Linked(i) => {
                    if i == 0 {
                        self.derivatives[0] += dir * 0.01 * dt * dt / dist;
                    }
                }
                _ => {}
            },
            1 => match self.state {
                SnakeState::Anchored(_) => {
                    self.derivatives[1] += dir * 0.01 * dt / dist;
                    self.anchor()
                }
                SnakeState::Linked(i) => {
                    if i == 1 {
                        self.derivatives[1] += dir * 0.01 * dt / dist;
                    }
                }
                _ => {}
            },
            2 => match self.state {
                SnakeState::Anchored(_) => {
                    self.derivatives[2] += dir * 0.01 * dt / dist;
                    self.anchor()
                }
                _ => {}
            },
            _ => {
                self.derivatives[2] += dir * 0.01 * dt / dist;
            }
        };
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SnakeState {
    Following(Pos2),
    Linked(usize),
    Anchored(Pos2),
    Drifting,
}

#[derive(Debug, Clone)]
pub struct Zone {
    centre: Pos2,
    radius: f32,
    inverted: bool,
    total: bool,
    persistent: bool,
    empty_col: ColSingle,
    held_col: ColSingle,
    set_col: Option<ColSingle>,
    state: ZoneState,
    last_out: std::time::Instant,
    time_req: std::time::Duration,
    progress: f32,
    action: Action,
    label: Option<String>,
}
impl Zone {
    fn new_goal(centre: Pos2, radius: f32, time_req: std::time::Duration) -> Self {
        Self {
            centre,
            radius,
            inverted: false,
            total: true,
            persistent: false,
            empty_col: ColSingle::LightRed,
            held_col: ColSingle::LightGreen,
            set_col: None,
            state: ZoneState::Empty,
            last_out: std::time::Instant::now(),
            time_req,
            progress: 0.,
            action: Action::Point,
            label: None,
        }
    }
    fn new_outzone(
        centre: Pos2,
        radius: f32,
        time_req: std::time::Duration,
        world_type: WorldType,
    ) -> Self {
        Self {
            centre,
            radius,
            inverted: true,
            total: false,
            persistent: false,
            empty_col: ColSingle::DarkGrey,
            held_col: ColSingle::DarkRed,
            set_col: None,
            state: ZoneState::Empty,
            last_out: std::time::Instant::now(),
            time_req,
            progress: 0.,
            action: Action::Reset(world_type),
            label: None,
        }
    }
    fn new_fail(
        centre: Pos2,
        radius: f32,
        time_req: std::time::Duration,
        world_type: WorldType,
    ) -> Self {
        Self {
            centre,
            radius,
            inverted: false,
            total: false,
            persistent: false,
            empty_col: ColSingle::DarkGrey,
            held_col: ColSingle::DarkRed,
            set_col: None,
            state: ZoneState::Empty,
            last_out: std::time::Instant::now(),
            time_req,
            progress: 0.,
            action: Action::Reset(world_type),
            label: None,
        }
    }
    fn new_option(centre: Pos2, radius: f32, action: Action, label: String) -> Self {
        Self {
            centre,
            radius,
            inverted: false,
            total: true,
            persistent: true,
            empty_col: ColSingle::LightBlue,
            held_col: ColSingle::LightGreen,
            set_col: Some(ColSingle::Gold),
            state: ZoneState::Empty,
            last_out: std::time::Instant::now(),
            time_req: std::time::Duration::from_secs_f32(1.5),
            progress: 0.,
            action,
            label: Some(label),
        }
    }

    fn step(&mut self, dt: f32) {
        match self.state {
            ZoneState::Empty => {
                if self.progress >= 1. {
                    self.progress = 0.
                } else {
                    self.progress =
                        f32::max(0., self.progress - 8. * dt / self.time_req.as_secs_f32())
                }
            }
            ZoneState::Held => self.progress += dt / self.time_req.as_secs_f32(),
            ZoneState::Set => {}
        }
    }

    pub fn current_col(&self) -> ColSingle {
        match self.state {
            ZoneState::Empty => self.empty_col,
            ZoneState::Held => self.held_col,
            ZoneState::Set => self.set_col.expect("No held col"),
        }
    }
    pub fn empty_col(&self) -> ColSingle {
        self.empty_col
    }
    pub fn held_col(&self) -> ColSingle {
        self.held_col
    }
    pub fn set_col(&self) -> Option<ColSingle> {
        self.set_col
    }
    pub fn state(&self) -> ZoneState {
        self.state
    }
    pub fn label(&self) -> &Option<String> {
        &self.label
    }
    pub fn centre(&self) -> Pos2 {
        self.centre
    }
    pub fn radius(&self) -> f32 {
        self.radius
    }
    pub fn progress(&self) -> f32 {
        self.progress
    }
    pub fn inverted(&self) -> bool {
        self.inverted
    }

    fn is_complete(&mut self, snake: &Snake) -> Option<Action> {
        // if total and all in the right place, or not total and one in the right place
        if (self.total
            && (0..snake.order + 1)
                .all(|n| ((snake.npos(n) - self.centre).length() < self.radius) ^ self.inverted))
            || (!self.total
                && (0..snake.order + 1).any(|n| {
                    ((snake.npos(n) - self.centre).length() < self.radius) ^ self.inverted
                }))
        {
            if self.state != ZoneState::Set {
                if self.progress() > 1. {
                    self.state = ZoneState::Set;
                    return Some(self.action);
                } else {
                    self.state = ZoneState::Held;
                }
            }
        } else {
            self.state = ZoneState::Empty;
            self.last_out = std::time::Instant::now();
        }
        None
    }
    fn is_safe(&self) -> bool {
        match self.action {
            Action::Reset(_) => false,
            _ => true,
        }
    }
    fn is_empty(&self) -> bool {
        self.state == ZoneState::Empty
    }
    fn is_held(&self) -> bool {
        self.state == ZoneState::Held
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ZoneState {
    Empty,
    Held,
    Set,
}

#[derive(Debug, Clone)]
pub struct Hazard {
    centre: Pos2,
    radius: f32,
    col: ColSingle,
    interaction: Interaction,
}
impl Hazard {
    fn new_attractor(centre: Pos2, radius: f32, strength: f32) -> Self {
        Self {
            centre,
            radius,
            col: ColSingle::Black,
            interaction: Interaction::Attract(strength),
        }
    }

    fn interact(&self, snake: &mut Snake, dt: f32) {
        match self.interaction {
            Interaction::Attract(strength) => {
                let dir = self.centre - snake.derivatives[0].to_pos2();
                let dist = (0.001 as f32).max(dir.length_sq());
                let dir = dir.normalized();
                match snake.order {
                    0 => match snake.state {
                        SnakeState::Anchored(_) => {
                            snake.derivatives[0] += dir * strength * dt * dt / dist;
                            snake.anchor()
                        }
                        SnakeState::Linked(i) => {
                            if i == 0 {
                                snake.derivatives[0] += dir * strength * dt * dt / dist;
                            }
                        }
                        _ => {}
                    },
                    1 => match snake.state {
                        SnakeState::Anchored(_) => {
                            snake.derivatives[1] += dir * strength * dt / dist;
                            snake.anchor()
                        }
                        SnakeState::Linked(i) => {
                            if i == 1 {
                                snake.derivatives[1] += dir * strength * dt / dist;
                            }
                        }
                        _ => {}
                    },
                    2 => match snake.state {
                        SnakeState::Anchored(_) => {
                            snake.derivatives[2] += dir * strength * dt / dist;
                            snake.anchor()
                        }
                        _ => {}
                    },
                    _ => {
                        snake.derivatives[2] += dir * strength * dt / dist;
                    }
                };
            }
        }
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
#[derive(Debug, Copy, Clone, PartialEq)]
enum Interaction {
    Attract(f32),
}

#[derive(Debug, Clone)]
pub struct World {
    world_type: WorldType,
    time: std::time::Duration,
    zones: Vec<Zone>,
    hazards: Vec<Hazard>,
    snake: Snake,
    guests: HashMap<u8, Snake>,
    friction: f32,
}
impl World {
    pub fn new() -> Self {
        Self {
            world_type: WorldType::Debug,
            time: std::time::Duration::from_secs(0),
            zones: vec![],
            hazards: vec![],
            snake: Snake::new(0, 0),
            guests: HashMap::new(),
            friction: 0.0001,
        }
    }

    pub fn to_type(&mut self, world_type: WorldType) {
        self.to_type_internal(world_type, false)
    }
    pub fn to_type_move(&mut self, world_type: WorldType) {
        self.to_type_internal(world_type, true)
    }
    fn to_type_internal(&mut self, world_type: WorldType, moving: bool) {
        self.world_type = world_type;
        self.time = std::time::Duration::from_secs(0);
        self.zones = vec![Zone::new_outzone(
            pos2(0., 0.) as Pos2,
            1.,
            std::time::Duration::from_secs(5),
            if world_type.is_playfield() {
                WorldType::MainMenu
            } else if world_type.is_arena() {
                world_type
            } else {
                world_type
            },
        )];
        self.hazards = vec![];

        let mut order = 0;
        let mut pos = pos2(0., 0.);
        let mut zones = vec![];
        let mut hazards = vec![];
        let unit = 0.25;
        let zone_rad = 0.1;
        match world_type {
            WorldType::Debug => todo!(),
            WorldType::Standard => {
                self.add_goal();
            }
            WorldType::Survival => {}
            WorldType::Gravity => {
                zones = vec![Zone::new_fail(
                    pos2(0., 0.),
                    2. * zone_rad,
                    std::time::Duration::from_secs(5),
                    WorldType::MainMenu,
                )];
                hazards = vec![Hazard::new_attractor(pos2(0., 0.), zone_rad / 3., 0.01)];
                order = 2;
                pos = pos2(0., 0.25);
            }
            WorldType::Training => {
                zones = vec![
                    Zone::new_option(
                        pos_rt(unit, PI),
                        zone_rad,
                        Action::Move(WorldType::MainMenu),
                        "Back".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI * 5. / 3.),
                        zone_rad,
                        Action::AdjustNodeCount(-1),
                        "Node -".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI * 1. / 3.),
                        zone_rad,
                        Action::AdjustNodeCount(1),
                        "Node +".to_string(),
                    ),
                ];
                order = 2;
            }
            WorldType::MainMenu => {
                zones = vec![
                    Zone::new_option(
                        pos2(0., 0.),
                        zone_rad,
                        Action::Move(WorldType::ModeSelect),
                        "Start".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI * 4. / 3.),
                        zone_rad,
                        Action::Move(WorldType::Training),
                        "Training".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI * 2. / 3.),
                        zone_rad,
                        Action::Move(WorldType::Options),
                        "Options".to_string(),
                    ),
                    Zone::new_option(pos_rt(unit, 0.), zone_rad, Action::Exit, "Exit".to_string()),
                ];
                order = 2;
                pos = pos_rt(unit, PI);
            }
            WorldType::ModeSelect => {
                zones = vec![
                    Zone::new_option(
                        pos_rt(unit, PI),
                        zone_rad,
                        Action::Move(WorldType::MainMenu),
                        "Back".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, 0.),
                        zone_rad,
                        Action::Reset(WorldType::Standard),
                        "Standard".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI * 4. / 3.),
                        zone_rad,
                        Action::Reset(WorldType::Survival),
                        "Survival".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI * 2. / 3.),
                        zone_rad,
                        Action::Reset(WorldType::Gravity),
                        "Gravity".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI * 1. / 3.),
                        zone_rad,
                        Action::JoinMultiplayer,
                        "Multiplayer".to_string(),
                    ),
                ];
            }
            WorldType::Options => {
                zones = vec![
                    Zone::new_option(
                        pos_rt(unit, PI),
                        zone_rad,
                        Action::Move(WorldType::MainMenu),
                        "Back".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI * 5. / 3.),
                        zone_rad,
                        Action::CycleColScheme,
                        "Cycle Scheme".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI * 1. / 3.),
                        zone_rad,
                        Action::ToggleLeadingTrail,
                        "Leading Trail".to_string(),
                    ),
                ];
                order = 2;
            }
            WorldType::ArenaMenu => {
                zones = vec![
                    Zone::new_option(
                        pos_rt(unit, PI),
                        zone_rad,
                        Action::LeaveMultiplayer,
                        "Back".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, 0.),
                        zone_rad,
                        Action::RegisterTeam(0),
                        "Spectate".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI * 3. / 2.),
                        zone_rad,
                        Action::RegisterTeam(1),
                        "Team 1".to_string(),
                    ),
                    Zone::new_option(
                        pos_rt(unit, PI / 2.),
                        zone_rad,
                        Action::RegisterTeam(2),
                        "Team 2".to_string(),
                    ),
                ];
                order = 2;
                self.snake.set_team(0);
                pos = pos_rt(0., 0.);
            }
            WorldType::Arena => {
                order = 4;
                pos = match self.snake.team {
                    1 => pos_rt(unit, PI * 3. / 2.),
                    2 => pos_rt(unit, PI / 2.),
                    _ => pos_rt(unit, 0.),
                }
            }
        }
        self.zones.append(&mut zones);
        self.hazards.append(&mut hazards);
        if !moving {
            self.snake.set_order(order);
            self.snake.reset(pos);
        }
    }

    pub fn world_type(&self) -> WorldType {
        self.world_type
    }
    pub fn snake(&self) -> &Snake {
        &self.snake
    }
    pub fn snake_mut(&mut self) -> &mut Snake {
        &mut self.snake
    }
    pub fn guests(&self) -> &HashMap<u8, Snake> {
        &self.guests
    }
    pub fn guests_mut(&mut self) -> &mut HashMap<u8, Snake> {
        &mut self.guests
    }
    pub fn hazards(&self) -> &Vec<Hazard> {
        &self.hazards
    }
    pub fn zones(&self) -> &Vec<Zone> {
        &self.zones
    }
    pub fn time(&self) -> std::time::Duration {
        self.time
    }

    pub fn add_goal(&mut self) {
        let mut rng = rand::thread_rng();
        let r = rng.gen::<f32>().sqrt() * 3. / 4.;
        let theta = rng.gen::<f32>() * std::f32::consts::PI * 2.;
        self.zones.push(Zone::new_goal(
            pos2(0., 0.) + r * vec2(theta.cos(), theta.sin()),
            0.075,
            std::time::Duration::from_secs(1),
        ))
    }

    pub fn step(&mut self, dt: f32) {
        self.snake.step(dt, self.friction);
        for hazard in &self.hazards {
            hazard.interact(&mut self.snake, dt);
        }
        for zone in &mut self.zones {
            zone.step(dt);
        }
        if self.world_type.is_arena() {
            for (_, guest) in &mut self.guests {
                if self.snake.team != guest.team && self.snake.team * guest.team != 0 {
                    self.snake.interact(guest, dt);
                    guest.interact(&self.snake, dt);
                }
            }
        }
        if !self.world_type.is_timed()
            || self
                .zones
                .iter()
                .all(|zone| zone.is_safe() || zone.is_empty())
        {
            self.time += std::time::Duration::from_secs_f32(dt);
        }
    }
    pub fn check(&mut self) -> Vec<Action> {
        let mut actions = vec![];
        self.zones.retain_mut(|zone| {
            if let Some(action) = zone.is_complete(&self.snake) {
                actions.push(action);
                return zone.persistent;
            };
            true
        });
        actions
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum WorldType {
    Debug,
    Standard,
    Survival,
    Gravity,
    Training,
    MainMenu,
    ModeSelect,
    Options,
    ArenaMenu,
    Arena,
}
impl WorldType {
    pub fn is_playfield(&self) -> bool {
        match self {
            WorldType::Standard | WorldType::Survival | WorldType::Gravity => true,
            _ => false,
        }
    }

    pub fn is_arena(&self) -> bool {
        match self {
            WorldType::Arena => true,
            _ => false,
        }
    }

    pub fn is_timed(&self) -> bool {
        match self {
            WorldType::Survival | WorldType::Gravity => true,
            _ => false,
        }
    }

    pub fn is_multiplayer(&self) -> bool {
        match self {
            WorldType::ArenaMenu | WorldType::Arena => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Action {
    Reset(WorldType),
    Move(WorldType),
    Point,
    ToggleLeadingTrail,
    AdjustNodeCount(isize),
    Exit,
    Dummy,
    JoinMultiplayer,
    LeaveMultiplayer,
    RegisterTeam(u8),
    SetColScheme(ColScheme),
    CycleColScheme,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SnakeData {
    id: u8,
    team: u8,
    order: usize,
    derivatives: Vec<Vec2>,
    state: SnakeState,
    spectrum: ColScheme,
    leading_trail: bool,
}
impl SnakeData {
    pub fn id(&self) -> u8 {
        self.id
    }
}

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
