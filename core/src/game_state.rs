use std::collections::HashMap;

use emath::{pos2, Pos2};

use crate::{
    snake::{LinkType, Snake, SnakePilot, SnakeState},
    world::{ResetTarget, Screen, ScreenId, ScreenIndex, WorldType},
    ColScheme, GameAction, NetworkAction, WorldDefinition,
};

pub struct ScoreBoard {
    pub(crate) score: usize,
    last_winner: Option<u8>,
}
impl ScoreBoard {}

pub struct GameState {
    world: WorldDefinition,
    current_screen: Screen,
    score_board: ScoreBoard,
    player: SnakePilot,
    guests: HashMap<u8, Snake>,
    time: std::time::Duration,
    exiting: bool,
    needs_save: bool,
}
impl GameState {
    pub fn new() -> Self {
        let world = WorldDefinition::new(WorldType::MainMenu);
        let current_screen = world.screen(ScreenId::Root);
        let mut player = SnakePilot::new(Snake::new(0, 2));
        current_screen.respawn(&mut player);
        let game_state = Self {
            world,
            current_screen,
            score_board: ScoreBoard {
                score: 0,
                last_winner: None,
            },
            player: player,
            guests: HashMap::new(),
            time: std::time::Duration::from_secs(0),
            exiting: false,
            needs_save: false,
        };
        game_state
    }
    pub fn snake(&self) -> &Snake {
        self.player.snake()
    }
    fn snake_mut(&mut self) -> &mut Snake {
        self.player.snake_mut()
    }
    pub fn guests(&self) -> &HashMap<u8, Snake> {
        &self.guests
    }
    pub fn guests_mut(&mut self) -> &mut HashMap<u8, Snake> {
        &mut self.guests
    }
    pub fn snake_by_id(&self, id: u8) -> &Snake {
        if self.snake().data().id() == id {
            &self.snake()
        } else {
            &self.guests.get(&id).expect("No such snake")
        }
    }
    pub fn snake_mut_by_id(&mut self, id: u8) -> &mut Snake {
        if self.snake().data().id() == id {
            self.snake_mut()
        } else {
            self.guests.get_mut(&id).expect("No such snake")
        }
    }
    pub fn set_snake_follow_target(&mut self, mpos: Pos2) {
        self.player.follow(mpos);
    }
    pub fn link_snake(&mut self, mpos: Pos2) {
        let (target_id, target_index, _) = self
            .guests()
            .iter()
            .chain([(&self.snake().id(), self.snake())])
            .map(|(&id, snake)| {
                (0..snake.data().order() + 1)
                    .map(move |a| (id, a, (mpos - snake.data().npos(a)).length_sq()))
            })
            .flatten()
            .min_by(|(_, _, a), (_, _, b)| a.total_cmp(b))
            .expect("No closest point");
        if target_id == self.snake().id() {
            self.link_snake_internal(target_index);
        } else {
            self.link_snake_other_internal(target_id, target_index);
        }
    }
    pub fn anchor_snake(&mut self) {
        self.player.anchor();
    }
    pub fn step(&mut self, dt: f32) {
        self.step_snake(dt);
        let Self {
            world,
            current_screen,
            player,
            guests,
            ..
        } = self;
        let screen = current_screen;
        screen.step(player, dt);
        if screen.is_arena() {
            for (_, guest) in guests {
                if player.snake().team() != guest.team()
                    && player.snake().team() * guest.team() != 0
                {
                    player.interact(guest, dt);
                    //guest.interact(&self.snake(), dt);
                }
            }
        }
        if !self.world.world_type().is_timed()
            || self
                .current_screen
                .zones()
                .iter()
                .all(|zone| zone.is_safe() || zone.is_empty())
        {
            self.time += std::time::Duration::from_secs_f32(dt);
        }
        match self.world.world_type() {
            WorldType::Standard => {
                if (self.snake().data().order() + 1) * (self.snake().data().order() + 1)
                    <= self.score_board.score
                {
                    self.player.add();
                }
            }
            WorldType::Survival | WorldType::Gravity => {
                self.score_board.score = self.time().as_secs() as usize;

                if (self.snake().data().order() + 1) * (self.snake().data().order() + 1)
                    <= self.score_board.score
                {
                    self.player.add();
                }
            }
            _ => (),
        }
    }
    fn step_snake(&mut self, dt: f32) {
        let state = *self.player.state();
        let friction = self.world.friction();
        let snake = self.snake_mut();
        snake.step_history();
        for i in (1..(snake.data().order() + 1)).rev() {
            let temp = snake.derivatives_mut()[i];
            snake.derivatives_mut()[i - 1] += temp * dt;
        }
        for i in &mut snake.derivatives_mut()[1..] {
            *i *= 1. - friction;
        }
        match state {
            SnakeState::Following(target) => {
                *snake.derivatives_mut().last_mut().unwrap() = target
                    - if snake.data().order() > 0 {
                        snake.data().npos(snake.data().order() - 1)
                    } else {
                        pos2(0., 0.)
                    };
            }
            SnakeState::Linked(link_type) => match link_type {
                LinkType::ToSelf(i) => {
                    *snake.derivatives_mut().last_mut().unwrap() = snake.data().npos(i)
                        - if snake.data().order() > 0 {
                            snake.data().npos(snake.data().order() - 1)
                        } else {
                            snake.data().npos(0)
                        };
                }
                LinkType::ToOther(id, i) => {
                    let target_snake = self.snake_by_id(id);
                    let target_pos = target_snake.data().npos(i);
                    let snake = self.snake_mut();
                    *snake.derivatives_mut().last_mut().unwrap() = target_pos
                        - if snake.data().order() > 0 {
                            snake.data().npos(snake.data().order() - 1)
                        } else {
                            snake.data().npos(0)
                        };
                }
            },
            SnakeState::Anchored(anchor) => {
                *snake.derivatives_mut().last_mut().unwrap() = anchor
                    - if snake.data().order() > 0 {
                        snake.data().npos(snake.data().order() - 1)
                    } else {
                        pos2(0., 0.)
                    };
            }
            SnakeState::Drifting => todo!(),
        }
    }
    fn link_snake_internal(&mut self, target: usize) {
        *self.player.state_mut() = SnakeState::Linked(LinkType::ToSelf(target));
        let snake = self.snake_mut();
        *snake.derivatives_mut().last_mut().unwrap() = snake.data().npos(target)
            - if snake.data().order() > 0 {
                snake.data().npos(snake.data().order() - 1)
            } else {
                pos2(0., 0.)
            };
    }
    fn link_snake_other_internal(&mut self, id: u8, target: usize) {
        let target_snake = self.snake_by_id(id);
        let target_vec = target_snake.data().npos(target)
            - if target_snake.data().order() > 0 {
                target_snake.data().npos(target_snake.data().order() - 1)
            } else {
                pos2(0., 0.)
            };
        let snake = self.snake_mut();
        *snake.derivatives_mut().last_mut().unwrap() = target_vec;
        *self.player.state_mut() = SnakeState::Linked(LinkType::ToOther(id, target));
    }
    pub fn set_snake_scheme(&mut self, scheme: ColScheme) {
        self.snake_mut().set_scheme(scheme);
        self.needs_save = true;
    }
    pub fn cycle_snake_scheme(&mut self) {
        self.snake_mut().cycle_scheme();
        self.needs_save = true;
    }
    pub fn set_snake_team(&mut self, team_id: u8) {
        self.snake_mut().set_team(team_id);
    }
    pub fn set_snake_id(&mut self, id: u8) {
        self.snake_mut().set_id(id);
    }
    pub fn toggle_leading_trail(&mut self) {
        self.snake_mut().toggle_leading_trail();
    }
    pub fn exit(&mut self) {
        self.exiting = true;
    }
    pub fn is_exiting(&self) -> bool {
        self.exiting
    }
    pub fn is_multiplayer(&self) -> bool {
        self.world.world_type().is_multiplayer()
    }
    pub fn is_arena(&self) -> bool {
        self.world.world_type().is_arena()
    }
    pub fn is_playfield(&self) -> bool {
        self.world.world_type().is_playfield()
    }
    pub fn world_type(&self) -> WorldType {
        self.world.world_type()
    }
    pub fn last_winner(&self) -> Option<u8> {
        self.score_board.last_winner
    }
    pub fn set_last_winner(&mut self, team_id: u8) {
        self.score_board.last_winner = Some(team_id);
    }

    pub fn check(&mut self) -> Vec<NetworkAction> {
        let actions = self.current_screen.check(&self.player);
        self.perform_actions(actions)
    }
    pub fn perform_actions(&mut self, actions: Vec<GameAction>) -> Vec<NetworkAction> {
        let Self {
            world,
            current_screen,
            player,
            exiting,
            score_board,
            guests,
            time,
            ..
        } = self;
        let mut net_actions = vec![];
        for action in actions {
            match action {
                GameAction::Respawn => current_screen.respawn(player),
                GameAction::Reset => {
                    score_board.score = 0;
                    player.snake_mut().set_team(0);
                    if world.world_type().is_multiplayer() {
                        net_actions.push(NetworkAction::RegisterTeam(0))
                    }
                    let target = current_screen.reset_target();
                    match target {
                        ResetTarget::SameScreen => {
                            *current_screen = world.screen(current_screen.id())
                        }
                        ResetTarget::WorldRoot(world_type) => {
                            *world = WorldDefinition::new(world_type);
                            *current_screen = world.screen(ScreenId::Root);
                        }
                    }
                }
                GameAction::Move(screen_id) => *current_screen = world.screen(screen_id),
                GameAction::World(world_type) => {
                    *world = WorldDefinition::new(world_type);
                    *current_screen = world.screen(ScreenId::Root);
                    *time = std::time::Duration::from_secs(0);
                }
                GameAction::Exit => *exiting = true,
                GameAction::Point => score_board.score += 1,
                GameAction::GenerateGoal => current_screen.add_goal_rand(),
                GameAction::JoinMultiplayer => net_actions.push(NetworkAction::JoinMultiplayer),
                GameAction::LeaveMultiplayer => {
                    guests.clear();
                    net_actions.push(NetworkAction::LeaveMultiplayer);
                }
                GameAction::RegisterTeam(id) => {
                    player.snake_mut().set_team(id);
                    net_actions.push(NetworkAction::RegisterTeam(id));
                }
                GameAction::SetColScheme(scheme) => player.snake_mut().set_scheme(scheme),
                GameAction::CycleColScheme => player.snake_mut().cycle_scheme(),
                GameAction::ToggleLeadingTrail => player.snake_mut().toggle_leading_trail(),
                GameAction::AdjustNodeCount(n) => {
                    for _ in 0..(n.abs()) {
                        if n < 0 {
                            player.remove();
                        } else {
                            player.add();
                        }
                    }
                }
            }
        }
        net_actions
    }
    pub fn score(&self) -> usize {
        self.score_board.score
    }
    pub fn reset_score(&mut self) {
        self.score_board.score = 0;
    }
    pub fn add_point(&mut self) {
        self.score_board.score += 1;
        if self.world_type() == WorldType::Standard {
            self.current_screen.add_goal_rand();
        }
    }
    pub fn time(&self) -> std::time::Duration {
        self.time
    }
    pub fn needs_save(&self) -> bool {
        self.needs_save
    }
    pub fn set_saved(&mut self) {
        self.needs_save = false
    }

    pub fn adjust_node_count(&mut self, n: isize) {
        for _ in 0..(n.abs()) {
            if n < 0 {
                self.player.remove();
            } else {
                self.player.add();
            }
        }
    }
    // fn to_world_internal(&mut self, world_type: WorldType, moving: bool) {
    //     self.world = World::new(world_type);
    //     if !moving {
    //         self.snake().data().set_order(order);
    //         self.player.snake.reset(pos);
    //     }
    // }
    pub fn screen(&self) -> &Screen {
        &self.current_screen
    }
    fn to_screen_internal(&mut self, world_type: WorldType, screen_id: ScreenId) {
        if world_type != self.world.world_type() {
            self.to_world(world_type)
        } else {
            self.to_screen(screen_id)
        }
    }
    fn to_world(&mut self, world_type: WorldType) {
        self.world = WorldDefinition::new(world_type)
    }
    fn to_screen(&mut self, screen_id: ScreenId) {
        self.current_screen = self.world.screen(screen_id);
    }
    pub fn reset(&mut self, screen_index: ScreenIndex) {
        self.to_screen_internal(screen_index.world(), screen_index.screen());
        let spawn_point = self.current_screen.spawner().clone();
        spawn_point.respawn(&mut self.player);
    }
    pub fn move_to_screen(&mut self, screen_index: ScreenIndex) {
        self.to_screen_internal(screen_index.world(), screen_index.screen())
    }
    // fn action(&mut self, action: Action) {
    //     match action {
    //         Action::Reset(screen_index) => {
    //             self.score = 0;
    //             if self.world.world_type().is_multiplayer() && self.world.world_type().is_arena() {
    //                 if let Some(socket) = socket.as_mut() {
    //                     socket
    //                         .send(Packet::reliable_unordered(
    //                             server,
    //                             Message::RegisterTeam(0).ser(),
    //                         ))
    //                         .expect("BAAAAD");
    //                     socket.manual_poll(std::time::Instant::now());
    //                 }
    //                 self.player.snake_mut().set_team(0);
    //             }
    //             self.to_screen_internal(screen_index.world(), screen_index.screen());
    //             self.world
    //                 .screen(self.current_screen)
    //                 .expect("No such screen")
    //                 .spawn_point()
    //                 .respawn(&mut self.player)
    //         }
    //         Action::Move(screen_index) => {
    //             self.to_screen_internal(screen_index.world(), screen_index.screen());
    //         }
    //         Action::Point => todo!(),
    //         Action::ToggleLeadingTrail => todo!(),
    //         Action::AdjustNodeCount(_) => todo!(),
    //         Action::Exit => todo!(),
    //         Action::Dummy => todo!(),
    //         Action::JoinMultiplayer => todo!(),
    //         Action::LeaveMultiplayer => todo!(),
    //         Action::RegisterTeam(_) => todo!(),
    //         Action::SetColScheme(_) => todo!(),
    //         Action::CycleColScheme => todo!(),
    //     }
    // }
}
