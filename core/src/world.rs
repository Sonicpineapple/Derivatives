use emath::{pos2, vec2, Pos2};
use itertools::Itertools;
use rand::prelude::*;
use std::{collections::HashMap, f32::consts::PI};

use crate::zone::Zone;
use crate::zone::ZoneDefinition;
use crate::GameAction;
use crate::{
    objects::{Hazard, Spawner},
    pos_rt,
    snake::SnakePilot,
    ColSingle, Text, TextType,
};

#[derive(Debug, Clone)]
pub struct ScreenDefinition {
    id: ScreenId,
    screen_type: ScreenType,
    zones: Vec<ZoneDefinition>,
    hazards: Vec<Hazard>,
    texts: Vec<Text>,
    spawn_point: Spawner,
    reset_target: ResetTarget,
}
impl ScreenDefinition {
    fn new(
        id: ScreenId,
        screen_type: ScreenType,
        mut zones: Vec<ZoneDefinition>,
        hazards: Vec<Hazard>,
        mut texts: Vec<Text>,
        spawn_point: Spawner,
        reset_target: ResetTarget,
    ) -> Self {
        let mut new_zones = vec![ZoneDefinition::killzone(
            pos2(0., 0.),
            1.,
            std::time::Duration::from_secs(5),
            true,
        )];
        new_zones.append(&mut zones);
        let zones = new_zones;
        let mut new_texts = vec![Text::new(pos2(0., 0.5), 2. / 7., TextType::SnakeOrder)];
        new_texts.append(&mut texts);
        let texts = new_texts;
        Self {
            id,
            screen_type,
            zones,
            hazards,
            texts,
            spawn_point,
            reset_target,
        }
    }
    fn new_menu(
        id: ScreenId,
        zones: Vec<ZoneDefinition>,
        texts: Vec<Text>,
        spawner: Spawner,
    ) -> Self {
        Self::new(
            id,
            ScreenType::Menu,
            zones,
            vec![],
            texts,
            spawner,
            ResetTarget::SameScreen,
        )
    }
    fn id(&self) -> ScreenId {
        self.id
    }
}

#[derive(Debug, Clone)]
pub struct Screen {
    id: ScreenId,
    screen_type: ScreenType,
    zones: Vec<Zone>,
    hazards: Vec<Hazard>,
    texts: Vec<Text>,
    spawner: Spawner,
    reset_target: ResetTarget,
}
impl Screen {
    fn from_definition(def: ScreenDefinition) -> Self {
        Self {
            id: def.id,
            screen_type: def.screen_type,
            zones: def
                .zones
                .into_iter()
                .map(Zone::from_definition)
                .collect_vec(),
            hazards: def.hazards,
            texts: def.texts,
            spawner: def.spawn_point,
            reset_target: def.reset_target,
        }
    }

    pub fn add_goal_rand(&mut self) {
        let mut rng = rand::thread_rng();
        let r = rng.gen::<f32>().sqrt() * 3. / 4.;
        let theta = rng.gen::<f32>() * std::f32::consts::PI * 2.;
        self.zones.push(Zone::from_definition(ZoneDefinition::goal(
            pos2(0., 0.) + r * vec2(theta.cos(), theta.sin()),
            0.075,
            std::time::Duration::from_secs(1),
        )));
    }

    pub fn hazards(&self) -> &Vec<Hazard> {
        &self.hazards
    }
    pub fn zones(&self) -> &Vec<Zone> {
        &self.zones
    }
    pub fn texts(&self) -> &Vec<Text> {
        &self.texts
    }
    pub fn spawner(&self) -> &Spawner {
        &self.spawner
    }
    pub fn respawn(&self, snake: &mut SnakePilot) {
        self.spawner.respawn(snake)
    }
    pub fn step(&mut self, snake: &mut SnakePilot, dt: f32) {
        for hazard in &self.hazards {
            hazard.interact(snake, dt);
        }
        for zone in &mut self.zones {
            zone.step(dt);
        }
    }
    pub fn check(&mut self, snake: &SnakePilot) -> Vec<GameAction> {
        let mut total_actions = vec![];
        self.zones.retain_mut(|zone| {
            if let Some(actions) = zone.check_complete(&snake.snake().data()) {
                total_actions.extend(actions);
                return zone.is_persistent();
            };
            true
        });
        total_actions
    }
    pub fn is_arena(&self) -> bool {
        self.screen_type == ScreenType::Arena
    }
    pub fn reset_target(&self) -> ResetTarget {
        self.reset_target.clone()
    }
    pub fn id(&self) -> ScreenId {
        self.id
    }
}

#[derive(Debug, Clone)]
pub struct WorldDefinition {
    world_type: WorldType,
    screens: HashMap<ScreenId, ScreenDefinition>,
    friction: f32,
}
impl WorldDefinition {
    pub fn new(world_type: WorldType) -> Self {
        let unit = 0.25;
        let zone_rad = 0.1;
        let mut screens = HashMap::new();
        let mut add_screen =
            |screen_def: ScreenDefinition| screens.insert(screen_def.id, screen_def);
        match world_type {
            WorldType::Debug => todo!(),
            WorldType::MainMenu => {
                add_screen(ScreenDefinition::new_menu(
                    ScreenId::Root,
                    vec![
                        ZoneDefinition::option(
                            pos2(0., 0.),
                            zone_rad,
                            vec![GameAction::Move(ScreenId::Branch("Mode Select"))],
                            "Start".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI * 4. / 3.),
                            zone_rad,
                            vec![GameAction::World(WorldType::Training)],
                            "Training".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI * 2. / 3.),
                            zone_rad,
                            vec![GameAction::Move(ScreenId::Branch("Options"))],
                            "Options".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, 0.),
                            zone_rad,
                            vec![GameAction::Exit],
                            "Exit".to_string(),
                        ),
                    ],
                    vec![],
                    Spawner::new(pos_rt(unit, PI), 2),
                ));
                add_screen(ScreenDefinition::new_menu(
                    ScreenId::Branch("Mode Select"),
                    vec![
                        ZoneDefinition::option(
                            pos_rt(unit, PI),
                            zone_rad,
                            vec![GameAction::Move(ScreenId::Root)],
                            "Back".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, 0.),
                            zone_rad,
                            vec![
                                GameAction::World(WorldType::Standard),
                                GameAction::Respawn,
                                GameAction::GenerateGoal,
                            ],
                            "Standard".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI * 4. / 3.),
                            zone_rad,
                            vec![GameAction::World(WorldType::Survival), GameAction::Respawn],
                            "Survival".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI * 2. / 3.),
                            zone_rad,
                            vec![GameAction::World(WorldType::Gravity), GameAction::Respawn],
                            "Gravity".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI * 1. / 3.),
                            zone_rad,
                            vec![
                                GameAction::JoinMultiplayer,
                                GameAction::World(WorldType::ArenaMenu),
                            ],
                            "Multiplayer".to_string(),
                        ),
                    ],
                    vec![],
                    Spawner::default(),
                ));
                add_screen(ScreenDefinition::new_menu(
                    ScreenId::Branch("Options"),
                    vec![
                        ZoneDefinition::option(
                            pos_rt(unit, PI),
                            zone_rad,
                            vec![GameAction::Move(ScreenId::Root)],
                            "Back".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI * 5. / 3.),
                            zone_rad,
                            vec![GameAction::CycleColScheme],
                            "Cycle Scheme".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI * 1. / 3.),
                            zone_rad,
                            vec![GameAction::ToggleLeadingTrail],
                            "Leading Trail".to_string(),
                        ),
                    ],
                    vec![],
                    Spawner::default(),
                ));
            }
            WorldType::ArenaMenu => {
                add_screen(ScreenDefinition::new_menu(
                    ScreenId::Root,
                    vec![
                        ZoneDefinition::option(
                            pos_rt(unit, PI),
                            zone_rad,
                            vec![
                                GameAction::LeaveMultiplayer,
                                GameAction::World(WorldType::MainMenu),
                            ],
                            "Back".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, 0.),
                            zone_rad,
                            vec![GameAction::RegisterTeam(0)],
                            "Spectate".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI * 3. / 2.),
                            zone_rad,
                            vec![GameAction::RegisterTeam(1)],
                            "Team 1".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI / 2.),
                            zone_rad,
                            vec![GameAction::RegisterTeam(2)],
                            "Team 2".to_string(),
                        ),
                    ],
                    vec![Text::last_winner()],
                    Spawner::default(),
                ));
            }
            WorldType::Training => {
                add_screen(ScreenDefinition::new(
                    ScreenId::Root,
                    ScreenType::Training,
                    vec![
                        ZoneDefinition::option(
                            pos_rt(unit, PI),
                            zone_rad,
                            vec![GameAction::World(WorldType::MainMenu)],
                            "Back".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI * 5. / 3.),
                            zone_rad,
                            vec![GameAction::AdjustNodeCount(-1)],
                            "Node -".to_string(),
                        ),
                        ZoneDefinition::option(
                            pos_rt(unit, PI * 1. / 3.),
                            zone_rad,
                            vec![GameAction::AdjustNodeCount(1)],
                            "Node +".to_string(),
                        ),
                    ],
                    vec![],
                    vec![],
                    Spawner::default(),
                    ResetTarget::SameScreen,
                ));
            }
            WorldType::Standard => {
                let screen = ScreenDefinition::new(
                    ScreenId::Root,
                    ScreenType::Standard,
                    vec![],
                    vec![],
                    vec![Text::order(), Text::score()],
                    Spawner::new(pos2(0., 0.), 0),
                    ResetTarget::WorldRoot(WorldType::MainMenu),
                );
                add_screen(screen);
            }
            WorldType::Survival => {
                add_screen(ScreenDefinition::new(
                    ScreenId::Root,
                    ScreenType::Survival,
                    vec![],
                    vec![],
                    vec![Text::order(), Text::score()],
                    Spawner::new(pos2(0., 0.), 0),
                    ResetTarget::WorldRoot(WorldType::MainMenu),
                ));
            }
            WorldType::Gravity => {
                add_screen(ScreenDefinition::new(
                    ScreenId::Root,
                    ScreenType::Survival,
                    vec![ZoneDefinition::killzone(
                        pos2(0., 0.),
                        2. * zone_rad,
                        std::time::Duration::from_secs(5),
                        false,
                    )],
                    vec![Hazard::new_attractor(pos2(0., 0.), zone_rad / 3., 0.02)],
                    vec![Text::order(), Text::score()],
                    Spawner::new(pos_rt(unit, PI), 0),
                    ResetTarget::WorldRoot(WorldType::MainMenu),
                ));
            }
            WorldType::Arena => {
                add_screen(ScreenDefinition::new(
                    ScreenId::Root,
                    ScreenType::Arena,
                    vec![],
                    vec![],
                    vec![Text::order()],
                    Spawner::new(pos2(0., 0.), 5),
                    ResetTarget::WorldRoot(WorldType::MainMenu),
                ));
            }
        };
        Self {
            world_type,
            screens,
            friction: 0.001,
        }
    }

    pub fn screen(&self, screen_id: ScreenId) -> Screen {
        Screen::from_definition(
            self.screens
                .get(&screen_id)
                .expect("No such screen")
                .clone(),
        )
    }
    pub fn world_type(&self) -> WorldType {
        self.world_type
    }
    pub fn friction(&self) -> f32 {
        self.friction
    }
}

#[derive(Debug, Clone)]
pub struct World {
    world_type: WorldType,
    screens: HashMap<ScreenId, Screen>,
    friction: f32,
}
impl World {
    pub fn world_type(&self) -> WorldType {
        self.world_type
    }
    pub fn friction(&self) -> f32 {
        self.friction
    }
    pub fn screen(&self, screen_id: &ScreenId) -> Option<&Screen> {
        self.screens.get(screen_id)
    }
    pub(crate) fn screen_mut(&mut self, screen_id: &ScreenId) -> Option<&mut Screen> {
        self.screens.get_mut(screen_id)
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum WorldType {
    Debug,
    MainMenu,
    ArenaMenu,
    Training,
    Standard,
    Survival,
    Gravity,
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
enum ScreenType {
    Menu,
    Training,
    Standard,
    Survival,
    Arena,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ScreenId {
    Root,
    Branch(&'static str),
}

#[derive(Debug, Clone)]
pub enum ResetTarget {
    SameScreen,
    WorldRoot(WorldType),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ScreenIndex {
    world: WorldType,
    screen: ScreenId,
}
impl ScreenIndex {
    pub fn new(world: WorldType, screen: ScreenId) -> Self {
        Self { world, screen }
    }
    pub fn world(&self) -> WorldType {
        self.world
    }
    pub fn screen(&self) -> ScreenId {
        self.screen
    }
}
