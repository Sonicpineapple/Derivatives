use emath::Pos2;

use crate::GameAction;
use crate::{ColSingle, SnakeData};

#[derive(Debug, Copy, Clone)]
pub enum ZoneType {
    Goal,
    Killzone,
    Option,
}
impl ZoneType {
    /// Whether the zone requires the entire snake to be inside it, or just one node
    fn is_total(&self) -> bool {
        match self {
            ZoneType::Goal | ZoneType::Option => true,
            _ => false,
        }
    }
    fn is_persistent(&self) -> bool {
        match self {
            ZoneType::Goal => false,
            _ => true,
        }
    }
    fn is_safe(&self) -> bool {
        match self {
            ZoneType::Killzone => false,
            _ => true,
        }
    }
}
#[derive(Debug, Clone)]
pub struct ZoneStyle {
    empty_col: ColSingle,
    held_col: ColSingle,
    set_col: Option<ColSingle>,
}
impl ZoneStyle {
    fn from_type(zone_type: ZoneType) -> Self {
        match zone_type {
            ZoneType::Goal => Self {
                empty_col: ColSingle::LightRed,
                held_col: ColSingle::LightGreen,
                set_col: None,
            },
            ZoneType::Killzone => Self {
                empty_col: ColSingle::DarkGrey,
                held_col: ColSingle::DarkRed,
                set_col: None,
            },
            ZoneType::Option => Self {
                empty_col: ColSingle::LightBlue,
                held_col: ColSingle::LightGreen,
                set_col: Some(ColSingle::Gold),
            },
        }
    }
}
#[derive(Debug, Clone)]
pub struct ZoneDefinition {
    centre: Pos2,
    radius: f32,
    time_req: std::time::Duration,
    inverted: bool,
    actions: Vec<GameAction>,
    label: Option<String>,
    zone_type: ZoneType,
}
impl ZoneDefinition {
    pub(crate) fn goal(centre: Pos2, radius: f32, time_req: std::time::Duration) -> Self {
        Self {
            centre,
            radius,
            time_req,
            inverted: false,
            actions: vec![GameAction::Point, GameAction::GenerateGoal],
            label: None,
            zone_type: ZoneType::Goal,
        }
    }
    pub(crate) fn killzone(
        centre: Pos2,
        radius: f32,
        time_req: std::time::Duration,
        inverted: bool,
    ) -> Self {
        Self {
            centre,
            radius,
            time_req,
            inverted,
            actions: vec![GameAction::Reset, GameAction::Respawn],
            label: None,
            zone_type: ZoneType::Killzone,
        }
    }
    pub(crate) fn option(
        centre: Pos2,
        radius: f32,
        actions: Vec<GameAction>,
        label: String,
    ) -> Self {
        Self {
            centre,
            radius,
            time_req: std::time::Duration::from_secs_f32(1.5),
            inverted: false,
            actions,
            label: Some(label),
            zone_type: ZoneType::Option,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Zone {
    def: ZoneDefinition,
    style: ZoneStyle,
    state: ZoneState,
    last_out: std::time::Instant,
    progress: f32,
}
impl Zone {
    pub(crate) fn from_definition(def: ZoneDefinition) -> Self {
        let style = ZoneStyle::from_type(def.zone_type);
        Self {
            def,
            state: ZoneState::Empty,
            style,
            last_out: std::time::Instant::now(),
            progress: 0.,
        }
    }
    pub(crate) fn step(&mut self, dt: f32) {
        match self.state {
            ZoneState::Empty => {
                if self.progress >= 1. {
                    self.progress = 0.
                } else {
                    self.progress = f32::max(
                        0.,
                        self.progress - 8. * dt / self.def.time_req.as_secs_f32(),
                    )
                }
            }
            ZoneState::Held => self.progress += dt / self.def.time_req.as_secs_f32(),
            ZoneState::Set => {}
        }
    }

    pub fn current_col(&self) -> ColSingle {
        match self.state {
            ZoneState::Empty => self.style.empty_col,
            ZoneState::Held => self.style.held_col,
            ZoneState::Set => self.style.set_col.unwrap_or(self.style.held_col),
        }
    }
    // pub fn empty_col(&self) -> ColSingle {
    //     self.empty_col
    // }
    // pub fn held_col(&self) -> ColSingle {
    //     self.held_col
    // }
    // pub fn set_col(&self) -> Option<ColSingle> {
    //     self.set_col
    // }
    pub fn state(&self) -> ZoneState {
        self.state
    }
    pub fn label(&self) -> &Option<String> {
        &self.def.label
    }
    pub fn centre(&self) -> Pos2 {
        self.def.centre
    }
    pub fn radius(&self) -> f32 {
        self.def.radius
    }
    pub fn progress(&self) -> f32 {
        self.progress
    }
    pub fn inverted(&self) -> bool {
        self.def.inverted
    }
    /// If the zone is complete, returns action to perform
    pub fn check_complete(&mut self, snake: &SnakeData) -> Option<Vec<GameAction>> {
        // if total and all in the right place, or not total and one in the right place
        if (self.def.zone_type.is_total()
            && (0..snake.order() + 1).all(|n| {
                ((snake.npos(n) - self.def.centre).length() < self.def.radius) ^ self.def.inverted
            }))
            || (!self.def.zone_type.is_total()
                && (0..snake.order() + 1).any(|n| {
                    ((snake.npos(n) - self.def.centre).length() < self.def.radius)
                        ^ self.def.inverted
                }))
        {
            if self.state != ZoneState::Set {
                if self.progress() > 1. {
                    self.state = ZoneState::Set;
                    return Some(self.def.actions.clone());
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
    pub fn is_safe(&self) -> bool {
        self.def.zone_type.is_safe()
    }
    pub fn is_empty(&self) -> bool {
        self.state == ZoneState::Empty
    }
    pub fn is_held(&self) -> bool {
        self.state == ZoneState::Held
    }
    pub fn is_persistent(&self) -> bool {
        self.def.zone_type.is_persistent()
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ZoneState {
    Empty,
    Held,
    Set,
}
