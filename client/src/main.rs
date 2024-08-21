#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use eframe::egui;
use egui::{pos2, vec2, Pos2, Vec2};
use laminar::{Packet, Socket, SocketEvent};
use std::{
    net::ToSocketAddrs,
    sync::{Arc, Mutex},
};

use derivatives_core::{
    get_team_col, ColScheme, ColSingle, GameAction, GameState, Message, NetworkAction, Snake,
    SnakeTeam, Text, Value, WorldType,
};

fn main() -> eframe::Result<()> {
    let native_options = eframe::NativeOptions {
        follow_system_theme: false,
        ..Default::default()
    };
    eframe::run_native(
        "Window Title",
        native_options,
        Box::new(|cc| Box::new(App::new(cc))),
    )
}

struct NetworkConfig {
    server_ip: String,
    lobby_id: String,
}

struct App {
    game_state: Arc<Mutex<GameState>>,
    network_config: Arc<Mutex<NetworkConfig>>,
}
impl App {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut game_state = GameState::new();
        let mut network_config = NetworkConfig {
            server_ip: "".to_string(),
            lobby_id: "Main".to_string(),
        };
        if let Some(storage) = cc.storage {
            if let Some(scheme) = eframe::get_value(storage, "Scheme") {
                game_state.set_snake_scheme(scheme);
            }
            if let Some(lobby_id) = eframe::get_value(storage, "Lobby ID") {
                network_config.lobby_id = lobby_id;
            }
            if let Some(server_ip) = eframe::get_value(storage, "Server IP") {
                network_config.server_ip = server_ip;
            }
        }
        let game_state = Arc::new(Mutex::new(game_state));
        let network_config = Arc::new(Mutex::new(network_config));

        let game_state_ref = Arc::clone(&game_state);
        let network_config_ref = Arc::clone(&network_config);
        std::thread::spawn(move || {
            let addr = "0.0.0.0:11111";

            let mut socket: Option<Socket> = None;
            let mut server: Option<core::net::SocketAddr> = None;

            let tick_rate = std::time::Duration::from_secs_f64(1.0 / 60.0);
            let mut frame_time = std::time::Instant::now();

            loop {
                let mut game_state = game_state_ref.lock().unwrap();
                let network_config = network_config_ref.lock().unwrap();

                if game_state.is_multiplayer() {
                    if let Some(socket) = socket.as_mut() {
                        if let Some(server) = server {
                            socket.manual_poll(std::time::Instant::now());
                            while let Some(event) = socket.recv() {
                                match event {
                                    SocketEvent::Packet(packet) => {
                                        if packet.addr() == server {
                                            if let Ok(msg) = Message::deser(packet.payload()) {
                                                match msg {
                                                    Message::Id(id) => {
                                                        game_state.set_snake_id(id);
                                                        println!("Connected with id {}", id);
                                                    }
                                                    Message::Snake(snake_data) => {
                                                        if snake_data.id()
                                                            != game_state.snake().id()
                                                        {
                                                            if let Some(guest) = game_state
                                                                .guests_mut()
                                                                .get_mut(&snake_data.id())
                                                            {
                                                                guest.step_history();
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
                                                        if id != game_state.snake().id() {
                                                            game_state
                                                                .guests_mut()
                                                                .insert(id, Snake::new(id, 3));
                                                        }
                                                        println!("id {} joined", id);
                                                    }
                                                    Message::Leave(leave_id) => game_state
                                                        .guests_mut()
                                                        .retain(|&id, _| id != leave_id),
                                                    Message::StartArena => {
                                                        game_state.perform_actions(vec![
                                                            GameAction::World(WorldType::Arena),
                                                            GameAction::Respawn,
                                                        ]);
                                                    }
                                                    Message::EndArena(team_id) => {
                                                        game_state.set_last_winner(team_id);
                                                        game_state.set_snake_team(None);
                                                        game_state.perform_actions(vec![
                                                            GameAction::World(WorldType::ArenaMenu),
                                                            GameAction::Respawn,
                                                        ]);
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
                }

                let last_frame_time = std::mem::replace(&mut frame_time, std::time::Instant::now());
                let dt = (frame_time - last_frame_time).as_secs_f32();

                if !game_state.is_paused() {
                    // Physics step
                    game_state.step(dt);

                    // Game
                    for action in game_state.check() {
                        match action {
                            NetworkAction::RegisterTeam(team_id) => {
                                if game_state.is_multiplayer() {
                                    if let Some(socket) = socket.as_mut() {
                                        if let Some(server) = server {
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
                            }
                            NetworkAction::JoinMultiplayer => {
                                if let Some(addr_list) =
                                    network_config.server_ip.trim().to_socket_addrs().ok()
                                {
                                    server = addr_list.last();
                                }
                                if let Some(server) = server {
                                    let mut skt = Socket::bind(addr).expect("Bad");
                                    println!("Connected on {}", addr);
                                    skt.send(Packet::reliable_unordered(
                                        server,
                                        Message::Connect.ser(),
                                    ))
                                    .expect("BAAAAD");
                                    skt.send(Packet::reliable_unordered(
                                        server,
                                        Message::Lobby(network_config.lobby_id.clone()).ser(),
                                    ))
                                    .expect("BAAAAD");
                                    skt.manual_poll(std::time::Instant::now());
                                    socket = Some(skt);
                                } else {
                                    println!(
                                        "Invalid server address {}",
                                        network_config.server_ip.trim()
                                    )
                                }
                            }
                            NetworkAction::LeaveMultiplayer => {
                                if let Some(socket) = socket.as_mut() {
                                    if let Some(server) = server {
                                        println!("Disconnected");
                                        socket
                                            .send(Packet::reliable_unordered(
                                                server,
                                                Message::Disconnect.ser(),
                                            ))
                                            .expect("BAAAAD");
                                        socket.manual_poll(std::time::Instant::now());
                                    }
                                }
                                socket = None;
                            }
                        }
                    }
                }
                if game_state.is_multiplayer() {
                    if let Some(socket) = socket.as_mut() {
                        if let Some(server) = server {
                            let snake_data = game_state.snake().data().clone();
                            socket
                                .send(Packet::reliable_unordered(
                                    server,
                                    Message::Snake(snake_data).ser(),
                                ))
                                .expect("BAAAAD");
                            socket
                                .send(Packet::reliable_unordered(server, Message::Heartbeat.ser()))
                                .expect("BAAAAD");
                            socket.manual_poll(std::time::Instant::now());
                        }
                    }
                }

                drop(game_state);
                if frame_time.elapsed() < tick_rate {
                    std::thread::sleep(tick_rate - frame_time.elapsed());
                }
            }
        });
        Self {
            game_state,
            network_config,
        }
    }

    fn draw_state(&mut self, ui: &mut egui::Ui, trans: &dyn Fn(Pos2) -> Pos2, unit: f32) {
        let game_state = &mut self.game_state.lock().unwrap();
        let screen = game_state.screen();
        for text in screen.texts() {
            draw_text(game_state, text, ui, trans, unit);
        }
        for hazard in screen.hazards() {
            draw_hazard(hazard, ui, trans, unit);
        }
        for zone in screen.zones() {
            draw_zone(zone, ui, trans, unit);
        }
        for guest in game_state.guests().values() {
            let gamma_mult =
                if game_state.is_multiplayer() && guest.team() == Some(SnakeTeam::Spectator) {
                    0.5
                } else {
                    1.
                };
            draw_snake(guest, ui, trans, unit, gamma_mult);
        }
        let gamma_mult =
            if game_state.is_arena() && game_state.snake().team() == Some(SnakeTeam::Spectator) {
                0.25
            } else {
                1.
            };
        draw_snake(game_state.snake(), ui, trans, unit, gamma_mult);
        if game_state.is_paused() {
            ui.painter().rect_filled(
                ui.painter().clip_rect(),
                0.,
                egui::Color32::from_black_alpha(128),
            );
            ui.painter().text(
                ui.clip_rect().center() + egui::vec2(0., -ui.clip_rect().height() / 3.),
                egui::Align2::CENTER_CENTER,
                "PAUSED",
                egui::FontId::monospace(50.),
                egui::Color32::GRAY,
            );
            if let Some(overlay) = game_state.overlay() {
                match overlay {
                    derivatives_core::OverlayType::LobbySelect => {
                        let rect = egui::Rect::from_center_size(
                            ui.clip_rect().center(),
                            ui.clip_rect().size() / 3.,
                        );
                        ui.painter().rect_filled(
                            rect,
                            5.,
                            egui::Color32::DARK_GRAY.gamma_multiply(0.4),
                        );
                        let mut network_config = self.network_config.lock().unwrap();
                        ui.allocate_ui_at_rect(rect.shrink(10.), |ui| {
                            ui.horizontal(|ui| {
                                ui.with_layout(
                                    egui::Layout::right_to_left(egui::Align::Min),
                                    |ui| {
                                        if ui.add(egui::Button::new("🗙")).clicked() {
                                            game_state.toggle_paused();
                                        }
                                    },
                                );
                            });
                            ui.add(egui::Label::new("Lobby Name"));
                            ui.add(
                                egui::TextEdit::singleline(&mut network_config.lobby_id)
                                    .hint_text("Main"),
                            );
                            ui.add(egui::Label::new("Server IP"));
                            ui.add(
                                egui::TextEdit::singleline(&mut network_config.server_ip)
                                    .hint_text("1.2.3.4:12345"),
                            );
                            if ui.button("Confirm").clicked() {
                                game_state.set_to_save();
                                game_state.toggle_paused()
                            }
                        });
                    }
                }
            }
        }
    }
}

fn transform(pos: Pos2, transform: (f32, Vec2)) -> Pos2 {
    (pos.to_vec2() * transform.0).to_pos2() + transform.1
}
fn inv_transform(pos: Pos2, transform: (f32, Vec2)) -> Pos2 {
    ((pos - transform.1).to_vec2() / transform.0).to_pos2()
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
    ui.painter()
        .circle_stroke(centre, radius, (edge_width, get_col(zone.current_col())));
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
    let history = snake.history();
    for (t, h) in history.history().iter().enumerate() {
        for (i, &e) in h.iter().enumerate() {
            if history.leading_trail() || i < snake.data().order() {
                let col = get_scheme(snake.data().scheme())(
                    i,
                    h.len() + if history.leading_trail() { 0 } else { 1 },
                )
                .gamma_multiply(gamma_mult * t as f32 / (4 * history.memory()) as f32);
                ui.painter().circle_filled(
                    trans(e),
                    t as f32 * node_rad / (3 * history.memory()) as f32,
                    col,
                );
            }
        }
    }
    for i in 1..snake.data().derivatives().len() {
        let col = get_col(if let Some(team) = snake.team() {
            get_team_col(team)
        } else {
            ColSingle::DarkGrey
        })
        .gamma_multiply(gamma_mult);
        ui.painter().line_segment(
            [trans(snake.data().npos(i - 1)), trans(snake.data().npos(i))],
            (line_width, col),
        );
    }
    for i in 0..snake.data().derivatives().len() {
        let col = get_scheme(snake.data().scheme())(i, snake.data().order() + 1)
            .gamma_multiply(gamma_mult);
        ui.painter()
            .circle_filled(trans(snake.data().npos(i)), node_rad, col);
    }
}
fn draw_text(
    game_state: &GameState,
    text: &Text,
    ui: &mut egui::Ui,
    trans: &dyn Fn(Pos2) -> Pos2,
    unit: f32,
) {
    let real_text = match text.text() {
        Value::Const(string) => string.to_string(),
        Value::Score => game_state.score().to_string(),
        Value::LastWinner => {
            if let Some(last_winner) = game_state.last_winner() {
                match last_winner {
                    SnakeTeam::Spectator => "Draw",
                    _ => "Win",
                }
                .to_string()
            } else {
                "".to_string()
            }
        }
        Value::SnakeOrder => game_state.snake().data().order().to_string(),
        Value::ArenaOrder => game_state.arena_order().to_string(),
    };
    let col = get_col(match text.text() {
        Value::LastWinner => {
            if let Some(team) = game_state.last_winner() {
                get_team_col(team)
            } else {
                ColSingle::DarkGrey
            }
        }
        _ => ColSingle::DarkGrey,
    });
    ui.put(
        egui::Rect::from_center_size(trans(text.position()), vec2(1., 1.) * (unit)),
        egui::widgets::Label::new(
            egui::RichText::new(real_text)
                .color(col)
                .size(unit * text.size()),
        ),
    );
}
fn get_col(col: ColSingle) -> egui::Color32 {
    match col {
        ColSingle::LightRed => egui::Color32::LIGHT_RED,
        ColSingle::LightGreen => egui::Color32::LIGHT_GREEN,
        ColSingle::LightBlue => egui::Color32::LIGHT_BLUE,
        ColSingle::DarkGrey => egui::Color32::DARK_GRAY,
        ColSingle::DarkRed => egui::Color32::DARK_RED,
        ColSingle::DarkBlue => egui::Color32::DARK_BLUE,
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
        ColScheme::Grays => {
            |i, n| colorous_to_egui(colorous::GREYS.eval_rational((n - 1 - i) % n, n))
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
        if game_state.needs_save() {
            eframe::set_value(
                _frame.storage_mut().expect("No storage"),
                "Scheme",
                &game_state.snake().data().scheme(),
            );
            let network_config = self.network_config.lock().unwrap();
            eframe::set_value(
                _frame.storage_mut().expect("No storage"),
                "Lobby ID",
                &network_config.lobby_id,
            );
            eframe::set_value(
                _frame.storage_mut().expect("No storage"),
                "Server IP",
                &network_config.server_ip,
            );
            game_state.set_saved()
        }
        if game_state.is_exiting() {
            ctx.send_viewport_cmd(egui::ViewportCommand::Close)
        }
        drop(game_state);
        egui::CentralPanel::default().show(ctx, |ui| {
            let mut game_state = self.game_state.lock().unwrap();
            ui.style_mut().interaction.selectable_labels = false;
            let rect = ui.available_rect_before_wrap();
            let (cen, size) = (rect.center(), rect.size());
            let unit = size.min_elem() / 2.;

            let trans_tup = (unit, cen.to_vec2());
            let trans = |pos| transform(pos, trans_tup);
            let itrans = |pos| inv_transform(pos, trans_tup);

            // Controls
            {
                if ui.input(|input| input.key_pressed(egui::Key::Escape)) {
                    game_state.toggle_paused()
                }
                if !game_state.is_paused() {
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
            }
            drop(game_state);
            // draw_state(&game_state, ui, &trans, unit);
            self.draw_state(ui, &trans, unit)
        });
        ctx.request_repaint();
    }
}
