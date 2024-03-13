#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use eframe::egui;
use egui::{pos2, vec2, Pos2, Vec2};
use laminar::{Packet, Socket, SocketEvent};
use std::sync::{Arc, Mutex};

use derivatives_core::{Action, ColScheme, ColSingle, Message, Snake, World, WorldType, ZoneState};

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
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut world = World::new();
        world.to_type(WorldType::MainMenu);
        if let Some(storage) = cc.storage {
            if let Some(scheme) = eframe::get_value(storage, "Scheme") {
                world.snake_mut().set_scheme(scheme);
            }
        }
        let game_state = Arc::new(Mutex::new(GameState {
            world,
            score: 0,
            exiting: false,
            needs_save: false,
        }));

        let game_state_ref = Arc::clone(&game_state);
        std::thread::spawn(move || {
            //for localhost
            // const SERVER: &str = "127.0.0.1:12345";
            //for webhost
            // const SERVER: &str = "Your.Server.Ip.Here:12345";
            const SERVER: &str = include_str!("../../server_ip.txt");

            //for localhost
            // let addr = "127.0.0.1:11111";
            //for webhost
            let addr = "0.0.0.0:11111";

            let server = SERVER.trim().parse().unwrap();
            let mut socket: Option<Socket> = None;

            let tick_rate = std::time::Duration::from_secs_f64(1.0 / 60.0);
            let mut frame_time = std::time::Instant::now();

            loop {
                let mut game_state = game_state_ref.lock().unwrap();

                if game_state.world.world_type().is_multiplayer() {
                    if let Some(socket) = socket.as_mut() {
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
                                                    if snake_data.id()
                                                        != game_state.world.snake().id()
                                                    {
                                                        if let Some(guest) = game_state
                                                            .world
                                                            .guests_mut()
                                                            .get_mut(&snake_data.id())
                                                        {
                                                            guest.set_data(snake_data);
                                                        } else {
                                                            println!(
                                                                "Guest {} doesn't exist",
                                                                snake_data.id()
                                                            )
                                                        }
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
                                                Message::StartArena => {
                                                    game_state.world.to_type(WorldType::Arena)
                                                }
                                                Message::EndArena => {
                                                    game_state.world.to_type(WorldType::ArenaMenu)
                                                }
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
                            if game_state.world.world_type().is_multiplayer()
                                && game_state.world.world_type().is_arena()
                            {
                                if let Some(socket) = socket.as_mut() {
                                    socket
                                        .send(Packet::reliable_unordered(
                                            server,
                                            Message::RegisterTeam(0).ser(),
                                        ))
                                        .expect("BAAAAD");
                                    socket.manual_poll(std::time::Instant::now());
                                }
                            }
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
                        Action::JoinMultiplayer => {
                            let mut skt = Socket::bind(addr).expect("Bad");
                            println!("Connected on {}", addr);
                            // let mut game_state = game_state_ref.lock().unwrap();
                            // if game_state.world.world_type().is_multiplayer() {}
                            skt.send(Packet::reliable_unordered(server, Message::Connect.ser()))
                                .expect("BAAAAD");
                            skt.manual_poll(std::time::Instant::now());
                            socket = Some(skt);
                            game_state.world.to_type_move(WorldType::ArenaMenu);
                        }
                        Action::LeaveMultiplayer => {
                            if let Some(socket) = socket.as_mut() {
                                socket
                                    .send(Packet::reliable_unordered(
                                        server,
                                        Message::Disconnect.ser(),
                                    ))
                                    .expect("BAAAAD");
                                socket.manual_poll(std::time::Instant::now());
                            }
                            socket = None;
                            game_state.world.guests_mut().clear();
                            game_state.world.to_type_move(WorldType::MainMenu);
                        }
                        Action::RegisterTeam(team_id) => {
                            if game_state.world.world_type().is_multiplayer() {
                                if let Some(socket) = socket.as_mut() {
                                    game_state.world.snake_mut().set_team(team_id);
                                    socket
                                        .send(Packet::reliable_unordered(
                                            server,
                                            Message::RegisterTeam(team_id).ser(),
                                        ))
                                        .expect("BAAAAD");
                                    socket.manual_poll(std::time::Instant::now());
                                }
                            }
                        }
                        Action::SetColScheme(scheme) => {
                            game_state.world.snake_mut().set_scheme(scheme);
                            game_state.needs_save = true;
                        }
                        Action::CycleColScheme => {
                            game_state.world.snake_mut().cycle_scheme();
                            game_state.needs_save = true;
                        }
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

                if game_state.world.world_type().is_multiplayer() {
                    if let Some(socket) = socket.as_mut() {
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
                    }
                }

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
    needs_save: bool,
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

fn draw_world(world: &World, ui: &mut egui::Ui, trans: &dyn Fn(Pos2) -> Pos2, unit: f32) {
    for hazard in world.hazards() {
        draw_hazard(hazard, ui, trans, unit);
    }
    for zone in world.zones() {
        draw_zone(zone, ui, trans, unit);
    }
    for guest in world.guests().values() {
        let gamma_mult = if world.world_type().is_multiplayer() && guest.team() == 0 {
            0.5
        } else {
            1.
        };
        draw_snake(guest, ui, trans, unit, gamma_mult);
    }
    let gamma_mult = if world.world_type().is_arena() && world.snake().team() == 0 {
        0.75
    } else {
        1.
    };
    draw_snake(world.snake(), ui, trans, unit, gamma_mult);
}
fn draw_hazard(
    hazard: &derivatives_core::Hazard,
    ui: &mut egui::Ui,
    trans: &dyn Fn(Pos2) -> Pos2,
    unit: f32,
) {
    let centre = trans(hazard.centre());
    let radius = hazard.radius() * unit;
    ui.painter()
        .circle_filled(centre, radius, get_col(hazard.col()));
}
fn draw_zone(
    zone: &derivatives_core::Zone,
    ui: &mut egui::Ui,
    trans: &dyn Fn(Pos2) -> Pos2,
    unit: f32,
) {
    let centre = trans(zone.centre());
    let radius = zone.radius() * unit;
    let edge_width = unit / 50.;
    let label_size = unit / 30.;
    if zone.progress() > 0. {
        let col = get_col(zone.current_col()).gamma_multiply(0.2);
        if zone.inverted() {
            let edge_width = radius * zone.progress();
            ui.painter()
                .circle_stroke(centre, radius - edge_width / 2., (edge_width, col));
        } else {
            ui.painter()
                .circle_filled(centre, (radius - edge_width / 2.) * zone.progress(), col);
        }
    }
    ui.painter().circle_stroke(
        centre,
        radius,
        (
            edge_width,
            match zone.state() {
                ZoneState::Empty => get_col(zone.empty_col()),
                ZoneState::Held => get_col(zone.held_col()),
                ZoneState::Set => get_col(zone.set_col().expect("No set colour")),
            },
        ),
    );
    if let Some(label) = zone.label() {
        ui.put(
            egui::Rect::from_center_size(centre, (2. * (radius - edge_width)) * vec2(1., 1.)),
            egui::widgets::Label::new(egui::RichText::new(label).size(label_size)),
        );
    }
}
fn draw_snake(
    snake: &Snake,
    ui: &mut egui::Ui,
    trans: &dyn Fn(Pos2) -> Pos2,
    unit: f32,
    gamma_mult: f32,
) {
    let node_rad = unit / 50.;
    let line_width = unit / 80.;
    for (t, h) in snake.history().iter().enumerate() {
        for (i, &e) in h.iter().enumerate() {
            if snake.leading_trail() || i < snake.order() {
                let col = get_scheme(snake.scheme())(
                    i,
                    h.len() + if snake.leading_trail() { 0 } else { 1 },
                )
                .gamma_multiply(gamma_mult * t as f32 / (4 * snake.memory()) as f32);
                ui.painter().circle_filled(
                    trans(e),
                    t as f32 * node_rad / (3 * snake.memory()) as f32,
                    col,
                );
            }
        }
    }
    for i in 1..snake.derivatives().len() {
        let col = match snake.team() {
            1 => egui::Color32::DARK_RED,
            2 => egui::Color32::DARK_BLUE,
            _ => egui::Color32::DARK_GRAY,
        }
        .gamma_multiply(gamma_mult);
        ui.painter().line_segment(
            [trans(snake.npos(i - 1)), trans(snake.npos(i))],
            (line_width, col),
        );
    }
    for i in 0..snake.derivatives().len() {
        let col = get_scheme(snake.scheme())(i, snake.order() + 1).gamma_multiply(gamma_mult);
        ui.painter()
            .circle_filled(trans(snake.npos(i)), node_rad, col);
    }
}
fn get_col(col: ColSingle) -> egui::Color32 {
    match col {
        ColSingle::LightRed => egui::Color32::LIGHT_RED,
        ColSingle::LightGreen => egui::Color32::LIGHT_GREEN,
        ColSingle::LightBlue => egui::Color32::LIGHT_BLUE,
        ColSingle::DarkGrey => egui::Color32::DARK_GRAY,
        ColSingle::DarkRed => egui::Color32::DARK_RED.gamma_multiply(1.),
        ColSingle::Gold => egui::Color32::GOLD,
        ColSingle::Black => egui::Color32::BLACK,
    }
}
fn get_scheme(scheme: ColScheme) -> Box<dyn Fn(usize, usize) -> egui::Color32> {
    Box::new(match scheme {
        ColScheme::Sinebow => |i, n| colorous_to_egui(colorous::SINEBOW.eval_rational(i, n)),
        ColScheme::Reds => {
            |i, n| colorous_to_egui(colorous::REDS.eval_rational((n - 1 - i) % n, n))
        }
        ColScheme::Greens => {
            |i, n| colorous_to_egui(colorous::GREENS.eval_rational((n - 1 - i) % n, n))
        }
        ColScheme::Blues => {
            |i, n| colorous_to_egui(colorous::BLUES.eval_rational((n - 1 - i) % n, n))
        }
        ColScheme::Purples => {
            |i, n| colorous_to_egui(colorous::PURPLES.eval_rational((n - 1 - i) % n, n))
        }
        ColScheme::Spectral => |i, n| colorous_to_egui(colorous::SPECTRAL.eval_rational(i, n)),
        ColScheme::Cool => |i, n| colorous_to_egui(colorous::COOL.eval_rational(i, n)),
        ColScheme::Warm => |i, n| colorous_to_egui(colorous::WARM.eval_rational(i, n)),
    })
}
fn colorous_to_egui(col: colorous::Color) -> egui::Color32 {
    egui::Color32::from_rgb(col.r, col.g, col.b)
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let mut game_state = self.game_state.lock().unwrap();
        if game_state.needs_save {
            eframe::set_value(
                _frame.storage_mut().expect("No storage"),
                "Scheme",
                &game_state.world.snake().scheme(),
            );
            game_state.needs_save = false;
        }
        if game_state.is_exiting() {
            ctx.send_viewport_cmd(egui::ViewportCommand::Close)
        }
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.style_mut().interaction.selectable_labels = false;
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
            draw_world(&game_state.world, ui, &trans, unit);
        });
        ctx.request_repaint();
    }
}
