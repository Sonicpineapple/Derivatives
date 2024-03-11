#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use eframe::egui;
use egui::{pos2, vec2, Pos2, Vec2};
use rand::prelude::*;
use std::{collections::VecDeque, f32::consts::PI};

use derivatives_core::{Action, SnakeState, World, WorldType};

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
        world.to_type(WorldType::MainMenu);
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
                        *self.world.snake_mut().state_mut() = SnakeState::Following(itrans(mpos));
                    };
                } else if ui.input(|input| input.pointer.secondary_pressed()) {
                    if let Some(mpos) = ctx.pointer_latest_pos() {
                        let mpos = itrans(mpos);
                        let target = (0..self.world.snake().order() + 1)
                            .min_by(|&a, &b| {
                                (mpos - self.world.snake().npos(a))
                                    .length_sq()
                                    .total_cmp(&(mpos - self.world.snake().npos(b)).length_sq())
                            })
                            .expect("No closest point");
                        *self.world.snake_mut().state_mut() = SnakeState::Linked(target);
                    };
                } else if ui.input(|input| input.pointer.primary_released()) {
                    self.world.snake_mut().anchor();
                }
                // if self.world.snake.order == 0 {
                //     self.fixed = false;
                // }
            }
            // Game
            for action in self.world.check() {
                match action {
                    Action::Reset(world_type) => {
                        self.score = 0;
                        self.world.to_type(world_type);
                    }
                    Action::Move(world_type) => {
                        self.world.to_type_move(world_type);
                    }
                    Action::Point => match self.world.world_type() {
                        WorldType::Standard => {
                            self.score += 1;
                            self.world.add_goal();
                        }
                        _ => todo!(),
                    },
                    Action::ToggleLeadingTrail => self.world.snake_mut().toggle_leading_trail(),
                    Action::AdjustNodeCount(n) => {
                        for _ in 0..(n.abs()) {
                            if n < 0 {
                                self.world.snake_mut().remove();
                            } else {
                                self.world.snake_mut().add();
                            }
                        }
                    }
                    Action::Exit => _frame.close(),
                    Action::Dummy => continue,
                }
            }
            match self.world.world_type() {
                WorldType::Standard => {
                    if (self.world.snake().order() + 1) * (self.world.snake().order() + 1)
                        <= self.score
                    {
                        self.world.snake_mut().add();
                    }
                }
                WorldType::Survival | WorldType::Gravity => {
                    self.score = self.world.time().as_secs() as usize;

                    if (self.world.snake().order() + 1) * (self.world.snake().order() + 1)
                        <= self.score
                    {
                        self.world.snake_mut().add();
                    }
                }
                _ => (),
            }
            // Drawing
            if self.world.world_type().is_playfield() {
                ui.put(
                    egui::Rect::from_center_size(trans(pos2(0., 0.)), vec2(1., 1.) * (unit)),
                    egui::widgets::Label::new(
                        egui::RichText::new(self.score.to_string())
                            .color(egui::Color32::DARK_GRAY)
                            .size(unit * 1. / 2.),
                    ),
                );
            }
            ui.put(
                egui::Rect::from_center_size(trans(pos2(0., 0.5)), vec2(1., 1.) * (unit)),
                egui::widgets::Label::new(
                    egui::RichText::new(self.world.snake().order().to_string())
                        .color(egui::Color32::DARK_GRAY)
                        .size(unit * 2. / 7.),
                ),
            );
            self.world.draw(ui, &trans, unit);
        });
        ctx.request_repaint();
    }
}
