#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use eframe::egui;
use egui::{pos2, vec2, Pos2, Vec2};
use laminar::{Packet, Socket, SocketEvent};
use std::sync::{Arc, Mutex};

use derivatives_core::{Action, Message, Snake, World, WorldType};

fn main() -> eframe::Result<()> {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "Window Title",
        native_options,
        Box::new(|cc| Box::new(App::new(cc))),
    )
}

struct App {
    game_state: Arc<Mutex<GameState>>,
}
impl App {
    fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        let mut world = World::new();
        world.to_type(WorldType::MainMenu);
        let game_state = Arc::new(Mutex::new(GameState {
            world,
            score: 0,
            exiting: false,
        }));

        let game_state_ref = Arc::clone(&game_state);
        std::thread::spawn(move || {
            const SERVER: &str = "127.0.0.1:12345";
            let addr = "127.0.0.1:11111";
            let mut socket = Socket::bind(addr).expect("Bad");
            println!("Connected on {}", addr);

            let server = SERVER.parse().unwrap();

            // let mut game_state = game_state_ref.lock().unwrap();
            // if game_state.world.world_type().is_multiplayer() {}
            socket
                .send(Packet::reliable_unordered(server, Message::Connect.ser()))
                .expect("BAAAAD");
            socket.manual_poll(std::time::Instant::now());

            let tick_rate = std::time::Duration::from_secs_f64(1.0 / 60.0);
            let mut frame_time = std::time::Instant::now();

            loop {
                let mut game_state = game_state_ref.lock().unwrap();

                socket.manual_poll(std::time::Instant::now());
                while let Some(event) = socket.recv() {
                    match event {
                        SocketEvent::Packet(packet) => {
                            if packet.addr() == server {
                                if let Ok(msg) = Message::deser(packet.payload()) {
                                    match msg {
                                        Message::Id(id) => {
                                            game_state.world.snake_mut().set_id(id);
                                            println!("Connected with id {}", id);
                                        }
                                        Message::Snake(snake_data) => {
                                            if snake_data.id() != game_state.world.snake().id() {
                                                game_state
                                                    .world
                                                    .guests_mut()
                                                    .get_mut(&snake_data.id())
                                                    .expect(
                                                        &("Guest ".to_owned()
                                                            + &snake_data.id().to_string()
                                                            + " doesn't exist"),
                                                    )
                                                    .set_data(snake_data);
                                            }
                                        }
                                        Message::Heartbeat => {}
                                        Message::Join(id) => {
                                            if id != game_state.world.snake().id() {
                                                game_state
                                                    .world
                                                    .guests_mut()
                                                    .insert(id, Snake::new(id, 3));
                                            }
                                            println!("id {} joined", id);
                                        }
                                        Message::Leave(leave_id) => game_state
                                            .world
                                            .guests_mut()
                                            .retain(|&id, _| id != leave_id),
                                        _ => todo!(),
                                    }
                                } else {
                                    println!("Garbage message");
                                }
                            } else {
                                println!("Unknown sender.");
                            }
                        }
                        SocketEvent::Timeout(_) => {
                            println!("Timed out")
                        }
                        _ => {
                            dbg!(event);
                        }
                    }
                }

                let last_frame_time = std::mem::replace(&mut frame_time, std::time::Instant::now());
                let dt = (frame_time - last_frame_time).as_secs_f32();

                // Physics step
                game_state.world.step(dt);

                // Game
                for action in game_state.world.check() {
                    match action {
                        Action::Reset(world_type) => {
                            game_state.score = 0;
                            game_state.world.to_type(world_type);
                        }
                        Action::Move(world_type) => {
                            game_state.world.to_type_move(world_type);
                        }
                        Action::Point => match game_state.world.world_type() {
                            WorldType::Standard => {
                                game_state.score += 1;
                                game_state.world.add_goal();
                            }
                            _ => todo!(),
                        },
                        Action::ToggleLeadingTrail => {
                            game_state.world.snake_mut().toggle_leading_trail()
                        }
                        Action::AdjustNodeCount(n) => {
                            for _ in 0..(n.abs()) {
                                if n < 0 {
                                    game_state.world.snake_mut().remove();
                                } else {
                                    game_state.world.snake_mut().add();
                                }
                            }
                        }
                        Action::Exit => game_state.exit(),
                        Action::Dummy => continue,
                    }
                }
                match game_state.world.world_type() {
                    WorldType::Standard => {
                        if (game_state.world.snake().order() + 1)
                            * (game_state.world.snake().order() + 1)
                            <= game_state.score
                        {
                            game_state.world.snake_mut().add();
                        }
                    }
                    WorldType::Survival | WorldType::Gravity => {
                        game_state.score = game_state.world.time().as_secs() as usize;

                        if (game_state.world.snake().order() + 1)
                            * (game_state.world.snake().order() + 1)
                            <= game_state.score
                        {
                            game_state.world.snake_mut().add();
                        }
                    }
                    _ => (),
                }

                let snake_data = game_state.world.snake().data();
                socket
                    .send(Packet::reliable_unordered(
                        server,
                        Message::Snake(snake_data).ser(),
                    ))
                    .expect("BAAAAD");
                socket
                    .send(Packet::reliable_unordered(server, Message::Heartbeat.ser()))
                    .expect("BAAAAD");
                // let mut msg = Message::Snake(game_state.world.snake().clone()).ser();
                // msg.append(&mut Message::Snake(game_state.world.snake().clone()).ser());
                // socket
                //     .send(Packet::reliable_unordered(server, msg))
                //     .expect("BAAAAD");
                socket.manual_poll(std::time::Instant::now());

                drop(game_state);
                if frame_time.elapsed() < tick_rate {
                    std::thread::sleep(tick_rate - frame_time.elapsed());
                }
            }
        });
        Self { game_state }
    }
}

struct GameState {
    world: World,

    score: usize,

    exiting: bool,
}
impl GameState {
    fn set_snake_follow_target(&mut self, mpos: Pos2) {
        self.world.snake_mut().follow(mpos);
    }
    fn link_snake(&mut self, mpos: Pos2) {
        let target = (0..self.world.snake().order() + 1)
            .min_by(|&a, &b| {
                (mpos - self.world.snake().npos(a))
                    .length_sq()
                    .total_cmp(&(mpos - self.world.snake().npos(b)).length_sq())
            })
            .expect("No closest point");
        self.world.snake_mut().link(target);
    }
    fn anchor_snake(&mut self) {
        self.world.snake_mut().anchor();
    }

    fn exit(&mut self) {
        self.exiting = true;
    }
    fn is_exiting(&self) -> bool {
        self.exiting
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
        let mut game_state = self.game_state.lock().unwrap();
        if game_state.is_exiting() {
            _frame.close();
        }
        egui::CentralPanel::default().show(ctx, |ui| {
            let rect = ui.available_rect_before_wrap();
            let (cen, size) = (rect.center(), rect.size());
            let unit = size.min_elem() / 2.;

            let trans_tup = (unit, cen.to_vec2());
            let trans = |pos| transform(pos, trans_tup);
            let itrans = |pos| inv_transform(pos, trans_tup);

            // Controls
            {
                if ui.input(|input| input.pointer.primary_down()) {
                    if let Some(mpos) = ctx.pointer_latest_pos() {
                        game_state.set_snake_follow_target(itrans(mpos));
                    };
                } else if ui.input(|input| input.pointer.secondary_pressed()) {
                    if let Some(mpos) = ctx.pointer_latest_pos() {
                        game_state.link_snake(itrans(mpos));
                    };
                } else if ui.input(|input| input.pointer.primary_released()) {
                    game_state.anchor_snake();
                }
            }
            // Drawing
            if game_state.world.world_type().is_playfield() {
                ui.put(
                    egui::Rect::from_center_size(trans(pos2(0., 0.)), vec2(1., 1.) * (unit)),
                    egui::widgets::Label::new(
                        egui::RichText::new(game_state.score.to_string())
                            .color(egui::Color32::DARK_GRAY)
                            .size(unit * 1. / 2.),
                    ),
                );
            }
            ui.put(
                egui::Rect::from_center_size(trans(pos2(0., 0.5)), vec2(1., 1.) * (unit)),
                egui::widgets::Label::new(
                    egui::RichText::new(game_state.world.snake().order().to_string())
                        .color(egui::Color32::DARK_GRAY)
                        .size(unit * 2. / 7.),
                ),
            );
            game_state.world.draw(ui, &trans, unit);
        });
        ctx.request_repaint();
    }
}
