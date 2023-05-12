#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use eframe::egui;
use egui::{pos2, vec2, Pos2, Vec2};
use rand::prelude::*;
use std::collections::VecDeque;

fn main() -> eframe::Result<()> {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "Window Title",
        native_options,
        Box::new(|cc| Box::new(App::new(cc))),
    )
}

struct App {
    world: World,

    score: usize,

    frame_time: std::time::Instant,
}
impl App {
    fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        let mut world = World::new();
        world.to_menu(2);
        Self {
            world,

            score: 0,

            frame_time: std::time::Instant::now(),
        }
    }
}

fn transform(pos: Pos2, transform: (f32, Vec2)) -> Pos2 {
    (pos.to_vec2() * transform.0).to_pos2() + transform.1
}
fn inv_transform(pos: Pos2, transform: (f32, Vec2)) -> Pos2 {
    ((pos - transform.1).to_vec2() / transform.0).to_pos2()
}

#[derive(Debug, Clone)]
struct Snake {
    order: usize,
    derivatives: Vec<Vec2>,
    state: SnakeState,
    memory: usize,
    history: VecDeque<Vec<Pos2>>,
    spectrum: colorous::Gradient,
    leading_trail: bool,
}
impl Snake {
    fn new(order: usize) -> Self {
        Self {
            order,
            derivatives: vec![vec2(0., 0.); order + 1],
            state: SnakeState::Anchored(pos2(0., 0.)),
            memory: 200,
            history: VecDeque::new(),
            spectrum: colorous::SINEBOW,
            leading_trail: false,
        }
    }
    fn draw(&self, ui: &mut egui::Ui, trans: &dyn Fn(Pos2) -> Pos2, unit: f32) {
        let node_rad = unit / 50.;
        let line_width = unit / 80.;
        for (t, h) in self.history.iter().enumerate() {
            for (i, &e) in h.iter().enumerate() {
                if self.leading_trail || i < self.order {
                    let col = self
                        .spectrum
                        .eval_rational(i, h.len() + if self.leading_trail { 0 } else { 1 });
                    let col = egui::Color32::from_rgba_unmultiplied(
                        col.r,
                        col.g,
                        col.b,
                        (t * 255 / (4 * self.memory)) as u8,
                    );
                    ui.painter().circle_filled(
                        trans(e),
                        t as f32 * node_rad / (3 * self.memory) as f32,
                        col,
                    );
                }
            }
        }
        for i in 1..self.derivatives.len() {
            ui.painter().line_segment(
                [trans(self.npos(i - 1)), trans(self.npos(i))],
                (line_width, egui::Color32::DARK_GRAY),
            )
        }
        for i in 0..self.derivatives.len() {
            let col = colorous::SINEBOW.eval_rational(i, self.order + 1);
            let col = egui::Color32::from_rgb(col.r, col.g, col.b);
            ui.painter()
                .circle_filled(trans(self.npos(i)), node_rad, col);
        }
    }
    fn step(&mut self, dt: f32, friction: f32) {
        self.history.push_back(
            (0..(self.order + if self.leading_trail { 1 } else { 0 }))
                .map(|i| self.npos(i))
                .collect(),
        );
        while self.history.len() > self.memory {
            self.history.pop_front();
        }
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

    fn add(&mut self) {
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
    fn remove(&mut self) {
        self.order -= 1;
        self.derivatives.pop();
    }
    fn set_order(&mut self, order: usize) {
        while self.order > order {
            self.remove();
        }
        while self.order < order {
            self.add();
        }
    }

    fn reset(&mut self, pos: Pos2) {
        self.derivatives = vec![vec2(0., 0.); self.order + 1];
        self.history = VecDeque::new();
        self.derivatives[0] = pos.to_vec2();
        self.anchor();
    }

    fn anchor(&mut self) {
        self.state = SnakeState::Anchored(self.npos(self.order));
    }

    fn npos(&self, n: usize) -> Pos2 {
        self.derivatives
            .iter()
            .take(n + 1)
            .fold(pos2(0., 0.), |a, &b| a + b)
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SnakeState {
    Following(Pos2),
    Linked(usize),
    Anchored(Pos2),
    Drifting,
}

#[derive(Debug, Clone)]
struct Zone {
    centre: Pos2,
    radius: f32,
    inverted: bool,
    persistent: bool,
    empty_col: egui::Color32,
    held_col: egui::Color32,
    set_col: Option<egui::Color32>,
    state: ZoneState,
    last_out: std::time::Instant,
    time_req: std::time::Duration,
    action: Action,
    label: Option<String>,
}
impl Zone {
    fn new_goal(centre: Pos2, radius: f32, time_req: std::time::Duration) -> Self {
        Self {
            centre,
            radius,
            inverted: false,
            persistent: false,
            empty_col: egui::Color32::LIGHT_RED,
            held_col: egui::Color32::LIGHT_GREEN,
            set_col: None,
            state: ZoneState::Empty,
            last_out: std::time::Instant::now(),
            time_req,
            action: Action::Point,
            label: None,
        }
    }
    fn new_fail(centre: Pos2, radius: f32, time_req: std::time::Duration) -> Self {
        Self {
            centre,
            radius,
            inverted: true,
            persistent: false,
            empty_col: egui::Color32::DARK_GRAY,
            held_col: egui::Color32::DARK_RED.gamma_multiply(0.2),
            set_col: None,
            state: ZoneState::Empty,
            last_out: std::time::Instant::now(),
            time_req,
            action: Action::Reset,
            label: None,
        }
    }
    fn new_option(centre: Pos2, radius: f32, action: Action, label: String) -> Self {
        Self {
            centre,
            radius,
            inverted: false,
            persistent: true,
            empty_col: egui::Color32::LIGHT_BLUE,
            held_col: egui::Color32::LIGHT_GREEN,
            set_col: Some(egui::Color32::GOLD),
            state: ZoneState::Empty,
            last_out: std::time::Instant::now(),
            time_req: std::time::Duration::from_secs_f32(1.5),
            action,
            label: Some(label),
        }
    }

    fn draw(&self, ui: &mut egui::Ui, trans: &dyn Fn(Pos2) -> Pos2, unit: f32) {
        let centre = trans(self.centre);
        let radius = self.radius * unit;
        let edge_width = unit / 50.;
        let label_size = unit / 25.;
        ui.painter().circle_stroke(
            centre,
            radius,
            (
                edge_width,
                match self.state {
                    ZoneState::Empty => self.empty_col,
                    ZoneState::Held => self.held_col,
                    ZoneState::Set => self.set_col.expect("No set colour"),
                },
            ),
        );
        if let Some(label) = &self.label {
            ui.put(
                egui::Rect::from_center_size(centre, 1.5 * vec2(radius, radius)),
                egui::widgets::Label::new(egui::RichText::new(label).size(label_size)),
            );
        }
    }

    fn is_complete(&mut self, snake: &Snake) -> Option<Action> {
        if self.inverted
            != (0..snake.order + 1).all(|n| (snake.npos(n) - self.centre).length() < self.radius)
        {
            if self.state != ZoneState::Set {
                if std::time::Instant::now().duration_since(self.last_out) > self.time_req {
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
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum ZoneState {
    Empty,
    Held,
    Set,
}

#[derive(Debug, Clone)]
struct World {
    world_type: WorldType,
    time: std::time::Duration,
    zones: Vec<Zone>,
    snake: Snake,
    friction: f32,
}
impl World {
    fn new() -> Self {
        Self {
            world_type: WorldType::Debug,
            time: std::time::Duration::from_secs(0),
            zones: vec![],
            snake: Snake::new(0),
            friction: 0.0001,
        }
    }
    fn to_standard(&mut self, order: usize) {
        self.world_type = WorldType::Standard;
        self.time = std::time::Duration::from_secs(0);
        self.zones = vec![Zone::new_fail(
            pos2(0., 0.) as Pos2,
            1.,
            std::time::Duration::from_secs(5),
        )];
        self.snake.set_order(order);
        self.snake.reset(pos2(0., 0.));
        self.add_goal();
    }
    fn to_survival(&mut self, order: usize) {
        self.world_type = WorldType::Survival;
        self.time = std::time::Duration::from_secs(0);
        self.zones = vec![Zone::new_fail(
            pos2(0., 0.) as Pos2,
            1.,
            std::time::Duration::from_secs(5),
        )];
        self.snake.set_order(order);
        self.snake.reset(pos2(0., 0.));
    }
    fn to_menu(&mut self, order: usize) {
        self.world_type = WorldType::Menu;
        self.time = std::time::Duration::from_secs(0);
        self.zones = vec![
            Zone::new_fail(pos2(0., 0.) as Pos2, 1., std::time::Duration::from_secs(5)),
            Zone::new_option(pos2(0., 0.), 0.1, Action::Action(0), "Start".to_string()),
            Zone::new_option(
                pos2(-0.25, 0.1),
                0.1,
                Action::Action(1),
                "Survival".to_string(),
            ),
            Zone::new_option(
                pos2(0.25, 0.1),
                0.1,
                Action::Action(2),
                "Options".to_string(),
            ),
            Zone::new_option(
                pos2(0., -0.27),
                0.1,
                Action::Action(3),
                "Training".to_string(),
            ),
        ];
        self.snake.set_order(order);
        self.snake.reset(pos2(0., 0.5));
    }
    fn to_options(&mut self, order: usize) {
        self.world_type = WorldType::Options;
        self.time = std::time::Duration::from_secs(0);
        self.zones = vec![
            Zone::new_fail(pos2(0., 0.) as Pos2, 1., std::time::Duration::from_secs(5)),
            Zone::new_option(pos2(0., 0.), 0.1, Action::Action(0), "Back".to_string()),
            Zone::new_option(
                pos2(-0.25, 0.1),
                0.1,
                Action::Action(1),
                "Dummy".to_string(),
            ),
            Zone::new_option(
                pos2(0.25, 0.1),
                0.1,
                Action::Action(2),
                "Leading Trail".to_string(),
            ),
        ];
        self.snake.set_order(order);
        self.snake.reset(pos2(0., 0.5));
    }
    fn to_training(&mut self, order: usize) {
        self.world_type = WorldType::Training;
        self.time = std::time::Duration::from_secs(0);
        self.zones = vec![
            Zone::new_fail(pos2(0., 0.) as Pos2, 1., std::time::Duration::from_secs(5)),
            Zone::new_option(pos2(0., 0.), 0.1, Action::Action(0), "Back".to_string()),
            Zone::new_option(
                pos2(-0.25, 0.1),
                0.1,
                Action::Action(1),
                "Node -".to_string(),
            ),
            Zone::new_option(
                pos2(0.25, 0.1),
                0.1,
                Action::Action(2),
                "Node +".to_string(),
            ),
        ];
        self.snake.set_order(order);
        self.snake.reset(pos2(0., 0.5));
    }

    fn add_goal(&mut self) {
        let mut rng = rand::thread_rng();
        let r = rng.gen::<f32>().sqrt() * 3. / 4.;
        let theta = rng.gen::<f32>() * std::f32::consts::PI * 2.;
        self.zones.push(Zone::new_goal(
            pos2(0., 0.) + r * vec2(theta.cos(), theta.sin()),
            0.075,
            std::time::Duration::from_secs(1),
        ))
    }

    fn draw(&self, ui: &mut egui::Ui, trans: &dyn Fn(Pos2) -> Pos2, unit: f32) {
        for zone in &self.zones {
            zone.draw(ui, trans, unit);
        }
        self.snake.draw(ui, trans, unit);
    }

    fn step(&mut self, dt: f32) {
        self.snake.step(dt, self.friction);
        if self.world_type != WorldType::Survival
            || self
                .zones
                .iter()
                .all(|zone| zone.action != Action::Reset || zone.state == ZoneState::Empty)
        {
            self.time += std::time::Duration::from_secs_f32(dt);
        }
    }
    fn check(&mut self) -> Vec<Action> {
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
enum WorldType {
    Debug,
    Standard,
    Survival,
    Training,
    Menu,
    Options,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Action {
    Reset,
    Point,
    Action(usize),
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let last_frame = self.frame_time;
        self.frame_time = std::time::Instant::now();
        let dt = (self.frame_time - last_frame).as_secs_f32();
        egui::CentralPanel::default().show(ctx, |ui| {
            let rect = ui.available_rect_before_wrap();
            let (cen, size) = (rect.center(), rect.size());
            let unit = size.min_elem() / 2.;

            let trans_tup = (unit, cen.to_vec2());
            let trans = |pos| transform(pos, trans_tup);
            let itrans = |pos| inv_transform(pos, trans_tup);

            // Physics step
            self.world.step(dt);
            // Controls
            {
                if ui.input(|input| input.pointer.primary_down()) {
                    if let Some(mpos) = ctx.pointer_latest_pos() {
                        self.world.snake.state = SnakeState::Following(itrans(mpos));
                    };
                } else if ui.input(|input| input.pointer.secondary_pressed()) {
                    if let Some(mpos) = ctx.pointer_latest_pos() {
                        let mpos = itrans(mpos);
                        let target = (0..self.world.snake.order + 1)
                            .min_by(|&a, &b| {
                                (mpos - self.world.snake.npos(a))
                                    .length_sq()
                                    .total_cmp(&(mpos - self.world.snake.npos(b)).length_sq())
                            })
                            .expect("No closest point");
                        self.world.snake.state = SnakeState::Linked(target);
                    };
                } else if ui.input(|input| input.pointer.primary_released()) {
                    self.world.snake.anchor();
                }
                // if self.world.snake.order == 0 {
                //     self.fixed = false;
                // }
            }
            // Game
            match self.world.world_type {
                WorldType::Standard => {
                    for action in self.world.check() {
                        match action {
                            Action::Reset => {
                                self.score = 0;
                                self.world.to_menu(2);
                            }
                            Action::Point => {
                                self.score += 1;
                                self.world.add_goal()
                            }
                            Action::Action(_) => todo!(),
                        }
                    }

                    if (self.world.snake.order + 1) * (self.world.snake.order + 1) <= self.score {
                        self.world.snake.add();
                    }
                    // Drawing
                    {
                        ui.put(
                            egui::Rect::from_center_size(
                                trans(pos2(0., -0.25)),
                                vec2(1., 1.) * (unit),
                            ),
                            egui::widgets::Label::new(
                                egui::RichText::new(self.score.to_string())
                                    .color(egui::Color32::DARK_GRAY)
                                    .size(unit * 1. / 2.),
                            ),
                        );
                        ui.put(
                            egui::Rect::from_center_size(
                                trans(pos2(0., 0.2)),
                                vec2(1., 1.) * (unit),
                            ),
                            egui::widgets::Label::new(
                                egui::RichText::new(self.world.snake.order.to_string())
                                    .color(egui::Color32::DARK_GRAY)
                                    .size(unit * 2. / 7.),
                            ),
                        );
                        self.world.draw(ui, &trans, unit);
                    }
                }
                WorldType::Survival => {
                    for action in self.world.check() {
                        match action {
                            Action::Reset => {
                                self.score = 0;
                                self.world.to_menu(2);
                            }
                            Action::Point => {
                                todo!();
                            }
                            Action::Action(_) => todo!(),
                        }
                    }
                    self.score = self.world.time.as_secs() as usize;

                    if (self.world.snake.order + 1) * (self.world.snake.order + 1) <= self.score {
                        self.world.snake.add();
                    }
                    // Drawing
                    {
                        ui.put(
                            egui::Rect::from_center_size(
                                trans(pos2(0., -0.25)),
                                vec2(1., 1.) * (unit),
                            ),
                            egui::widgets::Label::new(
                                egui::RichText::new(self.score.to_string())
                                    .color(egui::Color32::DARK_GRAY)
                                    .size(unit * 1. / 2.),
                            ),
                        );
                        ui.put(
                            egui::Rect::from_center_size(
                                trans(pos2(0., 0.2)),
                                vec2(1., 1.) * (unit),
                            ),
                            egui::widgets::Label::new(
                                egui::RichText::new(self.world.snake.order.to_string())
                                    .color(egui::Color32::DARK_GRAY)
                                    .size(unit * 2. / 7.),
                            ),
                        );
                        self.world.draw(ui, &trans, unit);
                    }
                }
                WorldType::Menu => {
                    for action in self.world.check() {
                        match action {
                            Action::Reset => {
                                self.world.to_menu(2);
                            }
                            Action::Point => todo!(),
                            Action::Action(i) => match i {
                                0 => {
                                    self.world.to_standard(0);
                                }
                                1 => {
                                    self.world.to_survival(0);
                                }
                                2 => {
                                    self.world.to_options(2);
                                }
                                3 => {
                                    self.world.to_training(2);
                                }
                                _ => {
                                    todo!();
                                }
                            },
                        }
                    }
                    self.world.draw(ui, &trans, unit);
                }
                WorldType::Options => {
                    for action in self.world.check() {
                        match action {
                            Action::Reset => {
                                self.world.to_options(2);
                            }
                            Action::Point => todo!(),
                            Action::Action(i) => match i {
                                0 => {
                                    self.world.to_menu(2);
                                }
                                1 => {
                                    continue;
                                }
                                2 => {
                                    self.world.snake.leading_trail =
                                        !self.world.snake.leading_trail;
                                }
                                _ => {
                                    todo!();
                                }
                            },
                        }
                    }
                    self.world.draw(ui, &trans, unit);
                }
                WorldType::Debug => todo!(),
                WorldType::Training => {
                    for action in self.world.check() {
                        match action {
                            Action::Reset => {
                                self.world.to_training(2);
                            }
                            Action::Point => todo!(),
                            Action::Action(i) => match i {
                                0 => {
                                    self.world.to_menu(2);
                                }
                                1 => {
                                    self.world.snake.remove();
                                }
                                2 => {
                                    self.world.snake.add();
                                }
                                _ => {
                                    todo!();
                                }
                            },
                        }
                    }
                    ui.put(
                        egui::Rect::from_center_size(trans(pos2(0., 0.2)), vec2(1., 1.) * (unit)),
                        egui::widgets::Label::new(
                            egui::RichText::new(self.world.snake.order.to_string())
                                .color(egui::Color32::DARK_GRAY)
                                .size(unit * 2. / 7.),
                        ),
                    );
                    self.world.draw(ui, &trans, unit);
                }
            }
        });
        ctx.request_repaint();
    }
}
