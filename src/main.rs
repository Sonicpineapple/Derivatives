use rand::prelude::*;
use std::collections::VecDeque;

use eframe::egui;
use egui::{pos2, vec2, Pos2, Vec2};

fn main() -> eframe::Result<()> {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "Window Title",
        native_options,
        Box::new(|cc| Box::new(App::new(cc))),
    )
}

struct App {
    mpos: Pos2,

    world: World,

    fixed: bool,
    target: usize,
    score: usize,

    menu: bool,

    frame_time: std::time::Instant,
}

impl App {
    fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_visuals.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.
        let n = 0;
        Self {
            mpos: pos2(0., 0.5),

            world: World::menu(2),

            fixed: false,
            target: 0,
            score: 0,

            menu: true,

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
    memory: usize,
    history: VecDeque<Vec<Pos2>>,
    spectrum: colorous::Gradient,
}
impl Snake {
    fn new(order: usize) -> Self {
        Self {
            order,
            derivatives: vec![vec2(0., 0.); order + 1],
            memory: 200,
            history: VecDeque::new(),
            spectrum: colorous::SINEBOW,
        }
    }
    fn draw(&self, ui: &mut egui::Ui, leading_trail: bool, trans: &dyn Fn(Pos2) -> Pos2) {
        for (t, h) in self.history.iter().enumerate() {
            for (i, &e) in h.iter().enumerate() {
                if leading_trail || i < self.order {
                    let col = self.spectrum.eval_rational(i, h.len());
                    let col = egui::Color32::from_rgba_unmultiplied(
                        col.r,
                        col.g,
                        col.b,
                        (t * 255 / (4 * self.memory)) as u8,
                    );
                    ui.painter().circle_filled(
                        trans(e),
                        t as f32 * 5. / (3 * self.memory) as f32,
                        col,
                    );
                }
            }
        }
        for i in 1..self.derivatives.len() {
            ui.painter().line_segment(
                [trans(self.npos(i - 1)), trans(self.npos(i))],
                (3., egui::Color32::DARK_GRAY),
            )
        }
        for i in 0..self.derivatives.len() {
            let col = colorous::SINEBOW.eval_rational(i, self.order + 1);
            let col = egui::Color32::from_rgb(col.r, col.g, col.b);
            ui.painter().circle_filled(trans(self.npos(i)), 5., col);
        }
    }
    fn step(&mut self, dt: f32, friction: f32) {
        self.history
            .push_back((0..(self.order + 1)).map(|i| self.npos(i)).collect());
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
    }
    fn to_control(&mut self, target: Pos2) {
        *self.derivatives.last_mut().unwrap() = target
            - if self.order > 0 {
                self.npos(self.order - 1)
            } else {
                pos2(0., 0.)
            };
    }

    fn add(&mut self) {
        self.order += 1;
        self.derivatives.push(vec2(0., 0.));
    }
    fn remove(&mut self) {
        self.order -= 1;
        self.derivatives.pop();
    }

    fn npos(&self, n: usize) -> Pos2 {
        self.derivatives
            .iter()
            .take(n + 1)
            .fold(pos2(0., 0.), |a, &b| a + b)
    }
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

    fn draw(&self, ui: &mut egui::Ui, trans: &dyn Fn(Pos2) -> Pos2) {
        let centre = trans(self.centre);
        let radius = self.radius * trans(pos2(1., 0.)).distance(trans(pos2(0., 0.)));
        ui.painter().circle_stroke(
            centre,
            radius,
            (
                5.,
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
                egui::widgets::Label::new(label),
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
    zones: Vec<Zone>,
    snake: Snake,
    friction: f32,
    leading_trail: bool,
}
impl World {
    fn new(order: usize) -> Self {
        let mut world = Self {
            zones: vec![Zone::new_fail(
                pos2(0., 0.) as Pos2,
                1.,
                std::time::Duration::from_secs(5),
            )],
            snake: Snake::new(order),
            friction: 0.0001,
            leading_trail: false,
        };
        world.add_goal();
        world
    }
    fn menu(order: usize) -> Self {
        let mut snake = Snake::new(order);
        snake.derivatives[0] = vec2(0., 0.5);
        Self {
            zones: vec![
                Zone::new_fail(pos2(0., 0.) as Pos2, 1., std::time::Duration::from_secs(5)),
                Zone::new_option(pos2(0., 0.), 0.1, Action::Action(0), "Start".to_string()),
                Zone::new_option(
                    pos2(0.25, 0.1),
                    0.1,
                    Action::Action(1),
                    "Leading Trail".to_string(),
                ),
            ],
            snake,
            friction: 0.0001,
            leading_trail: false,
        }
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

    fn draw(&self, ui: &mut egui::Ui, trans: &dyn Fn(Pos2) -> Pos2) {
        for zone in &self.zones {
            zone.draw(ui, trans);
        }
        self.snake.draw(ui, self.leading_trail, trans);
    }

    fn step(&mut self, dt: f32) {
        self.snake.step(dt, self.friction);
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
            let trans_tup = (
                ui.available_size_before_wrap().min_elem() / 2.,
                ui.available_rect_before_wrap().center().to_vec2(),
            );
            let trans = |pos| transform(pos, trans_tup);
            let itrans = |pos| inv_transform(pos, trans_tup);

            // Physics step
            self.world.step(dt);
            // Controls
            {
                if ui.input(|input| input.pointer.primary_down()) {
                    self.fixed = false;
                    if let Some(mpos) = ctx.pointer_latest_pos() {
                        self.mpos = itrans(mpos);
                    };
                } else if ui.input(|input| input.pointer.secondary_pressed()) {
                    if let Some(mpos) = ctx.pointer_latest_pos() {
                        self.mpos = itrans(mpos);
                    };
                    self.fixed = !self.fixed;
                    if self.fixed {
                        self.target = (0..self.world.snake.order + 1)
                            .min_by(|&a, &b| {
                                (self.mpos - self.world.snake.npos(a))
                                    .length_sq()
                                    .total_cmp(&(self.mpos - self.world.snake.npos(b)).length_sq())
                            })
                            .expect("No closest point");
                    }
                }
                if self.world.snake.order == 0 {
                    self.fixed = false;
                }

                self.world.snake.to_control(if self.fixed {
                    self.world.snake.npos(self.target)
                } else {
                    self.mpos
                });
            }
            // Game
            if self.menu {
                for action in self.world.check() {
                    match action {
                        Action::Reset => {
                            self.mpos = pos2(0., 0.5);
                            self.world = World::menu(2);
                        }
                        Action::Point => todo!(),
                        Action::Action(i) => match i {
                            0 => {
                                self.menu = false;
                                self.world = World::new(0);
                            }
                            1 => {
                                self.world.leading_trail = !self.world.leading_trail;
                            }
                            _ => {
                                todo!();
                            }
                        },
                    }
                }
                self.world.draw(ui, &trans);
            } else {
                for action in self.world.check() {
                    match action {
                        Action::Reset => {
                            self.score = 0;
                            self.mpos = pos2(0., 0.5);
                            self.menu = true;
                            self.world = World::menu(2);
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
                    self.world.draw(ui, &trans);
                    ui.label(
                        egui::RichText::new(self.score.to_string())
                            .color(egui::Color32::LIGHT_GRAY)
                            .size(20.),
                    );
                    ui.label(
                        egui::RichText::new(self.world.snake.order.to_string())
                            .color(egui::Color32::LIGHT_GRAY)
                            .size(15.),
                    );
                }
            }
        });
        ctx.request_repaint();
    }
}
