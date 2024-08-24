use bimap::BiMap;
use itertools::Itertools;
use laminar::{ErrorKind, Packet, Socket, SocketEvent};
use std::{collections::HashMap, net::SocketAddr, thread};

use derivatives_core::{Message, SnakeTeam};

const SERVER: &str = "0.0.0.0:12345";

struct Lobby {
    players: Vec<u8>,
    teams: HashMap<u8, SnakeTeam>,
    arena_order: usize,
    game_in_progress: bool,
    last_loss: Option<std::time::Instant>,
}
impl Lobby {
    fn new() -> Self {
        Self {
            players: vec![],
            teams: HashMap::new(),
            arena_order: 5,
            game_in_progress: false,
            last_loss: None,
        }
    }
}

fn server() -> Result<(), ErrorKind> {
    let mut socket = Socket::bind(SERVER)?;
    let (sender, receiver) = (socket.get_packet_sender(), socket.get_event_receiver());
    let _thread = thread::spawn(move || socket.start_polling());
    // let mut next_id = 0;
    fn get_next_id(clients: &BiMap<u8, SocketAddr>) -> Option<u8> {
        for i in 0..=u8::MAX {
            if clients.get_by_left(&i).is_none() {
                return Some(i);
            }
        }
        None
    }
    let mut lobbies: HashMap<String, Lobby> = HashMap::new();
    let mut clients: BiMap<u8, SocketAddr> = BiMap::new();
    let mut client_lobbies: HashMap<u8, String> = HashMap::new();
    const WIN_MARGIN: std::time::Duration = std::time::Duration::new(2, 0);

    loop {
        let rec = receiver.recv();
        if let Ok(event) = rec {
            match event {
                SocketEvent::Packet(packet) => {
                    if let Ok(msg) = Message::deser(packet.payload()) {
                        match msg {
                            Message::Connect => {
                                let next_id = get_next_id(&clients);
                                if let Some(next_id) = next_id {
                                    sender
                                        .send(Packet::reliable_unordered(
                                            packet.addr(),
                                            Message::Id(next_id).ser(),
                                        ))
                                        .expect("This should send");
                                    println!("Assigned id {}", next_id);
                                    clients.insert(next_id, packet.addr());
                                    // dbg!(&clients);
                                    // next_id += 1;
                                } else {
                                    sender
                                        .send(Packet::reliable_unordered(
                                            packet.addr(),
                                            Message::Refuse("Server full".to_string()).ser(),
                                        ))
                                        .expect("This should send");
                                }
                            }
                            Message::Lobby(lobby_id) => {
                                if let Some(&client_id) = clients.get_by_right(&packet.addr()) {
                                    let lobby_id = lobby_id.to_lowercase();
                                    let lobby =
                                        lobbies.entry(lobby_id.clone()).or_insert(Lobby::new());
                                    lobby.players.push(client_id);
                                    client_lobbies.entry(client_id).or_insert(lobby_id.clone());
                                    for &id in &lobby.players {
                                        let addr = *clients.get_by_left(&id).expect("Bad id");
                                        if addr != packet.addr() {
                                            sender
                                                .send(Packet::reliable_unordered(
                                                    addr,
                                                    Message::Join(client_id).ser(),
                                                ))
                                                .expect("This should send");
                                            sender
                                                .send(Packet::reliable_unordered(
                                                    packet.addr(),
                                                    Message::Join(id).ser(),
                                                ))
                                                .expect("This should send");
                                        }
                                    }
                                    println!("Id {} joined lobby {}", client_id, lobby_id);
                                    if lobby.game_in_progress {
                                        lobby.teams.insert(client_id, SnakeTeam::Spectator);
                                        println!("Id {} joined team {}", client_id, 0);
                                        sender
                                            .send(Packet::reliable_unordered(
                                                packet.addr(),
                                                Message::StartArena.ser(),
                                            ))
                                            .expect("This should send");
                                    }
                                } else {
                                    println!("Attempted lobby join by address not assigned id");
                                }
                            }
                            Message::Snake(_) => {
                                if let Some(client_id) = clients.get_by_right(&packet.addr()) {
                                    let lobby = lobbies
                                        .get(client_lobbies.get(client_id).expect("Not in lobby"))
                                        .expect("Lobby doesn't exist");
                                    for player in &lobby.players {
                                        let addr = *clients.get_by_left(player).expect("Bad id");
                                        if addr != packet.addr() {
                                            sender
                                                .send(Packet::reliable_unordered(addr, msg.ser()))
                                                .expect("This should send");
                                        }
                                    }
                                } else {
                                    println!("Attempted snake share from address not assigned id")
                                }
                            }
                            Message::Disconnect => {
                                if let Some(&leave_id) = clients.get_by_right(&packet.addr()) {
                                    println!(
                                        "Client disconnected: {}, id {}",
                                        packet.addr(),
                                        leave_id
                                    );
                                    clients.retain(|_, &addr| addr != packet.addr());
                                    for lobby in lobbies.values_mut() {
                                        lobby.players.retain(|&id| id != leave_id);
                                        lobby.teams.retain(|&id, _| id != leave_id);
                                    }
                                    client_lobbies.retain(|&id, _| id != leave_id);
                                    for &addr in clients.right_values() {
                                        sender
                                            .send(Packet::reliable_unordered(
                                                addr,
                                                Message::Leave(leave_id).ser(),
                                            ))
                                            .expect("This should send");
                                    }
                                } else {
                                    println!("Attempted disconnect by address not assigned id");
                                }
                            }
                            Message::Heartbeat => sender
                                .send(Packet::reliable_unordered(
                                    packet.addr(),
                                    Message::Heartbeat.ser(),
                                ))
                                .expect("This should send"),
                            Message::RegisterTeam(team_id) => {
                                if let Some(&client_id) = clients.get_by_right(&packet.addr()) {
                                    let lobby = lobbies
                                        .get_mut(
                                            client_lobbies
                                                .get(&client_id)
                                                .expect("Not in lobby (oof)"),
                                        )
                                        .expect("Lobby isn't real it can't hurt you");
                                    if lobby.game_in_progress {
                                        lobby.last_loss = Some(std::time::Instant::now());
                                    }
                                    lobby.teams.insert(client_id, team_id);
                                    println!("Id {} joined team {}", client_id, team_id)
                                } else {
                                    println!(
                                        "Attempted team registration by address not assigned id"
                                    );
                                }
                            }
                            Message::AdjustArenaOrder(n) => {
                                if let Some(&client_id) = clients.get_by_right(&packet.addr()) {
                                    let lobby = lobbies
                                        .get_mut(
                                            client_lobbies
                                                .get(&client_id)
                                                .expect("Not in lobby (oof)"),
                                        )
                                        .expect("Lobby isn't real it can't hurt you");
                                    if !lobby.game_in_progress {
                                        lobby.arena_order =
                                            (lobby.arena_order as isize + n).max(3) as usize;
                                        for player in &lobby.players {
                                            let addr =
                                                *clients.get_by_left(player).expect("Bad id");
                                            sender
                                                .send(Packet::reliable_unordered(
                                                    addr,
                                                    Message::SetArenaOrder(lobby.arena_order).ser(),
                                                ))
                                                .expect("This should send");
                                        }
                                    }
                                }
                            }
                            _ => todo!(),
                        }
                        //dbg!(std::time::Instant::now());
                    } else {
                        println!("Garbage message")
                    }
                }
                SocketEvent::Timeout(address) => {
                    if let Some(&leave_id) = clients.get_by_right(&address) {
                        println!("Client timed out: {}, id {}", address, leave_id);
                        clients.retain(|_, &addr| addr != address);
                        for lobby in lobbies.values_mut() {
                            lobby.players.retain(|&id| id != leave_id);
                            lobby.teams.retain(|&id, _| id != leave_id);
                        }
                        client_lobbies.retain(|&id, _| id != leave_id);
                        for &addr in clients.right_values() {
                            sender
                                .send(Packet::reliable_unordered(
                                    addr,
                                    Message::Leave(leave_id).ser(),
                                ))
                                .expect("This should send");
                        }
                    } else {
                        println!("Unknown timeout {}", address);
                    }
                }
                _ => {
                    dbg!(event);
                }
            }
        } else {
            dbg!(rec);
        }

        for lobby in lobbies.values_mut() {
            let Lobby {
                players,
                teams,
                arena_order,
                game_in_progress,
                last_loss,
            } = lobby;
            let team_players_left: Vec<&SnakeTeam> = teams
                .values()
                .filter(|&&team_id| team_id != SnakeTeam::Spectator)
                .collect();

            if !*game_in_progress
                && players.iter().all(|id| teams.contains_key(id))
                && !team_players_left.iter().all_equal()
            {
                for player in players {
                    let &addr = clients.get_by_left(&player).expect("Player doesn't exist");
                    sender
                        .send(Packet::reliable_unordered(addr, Message::StartArena.ser()))
                        .expect("This should send");
                }
                *game_in_progress = true;
                println!("Game started");
            } else if *game_in_progress && team_players_left.iter().all_equal() {
                if let Some(last_loss) = last_loss {
                    if std::time::Instant::now().duration_since(*last_loss) > WIN_MARGIN {
                        let winning_team = if let Some(&&team) = team_players_left.first() {
                            team
                        } else {
                            SnakeTeam::Spectator
                        };
                        for player in players {
                            let &addr = clients.get_by_left(&player).expect("Player doesn't exist");
                            sender
                                .send(Packet::reliable_unordered(
                                    addr,
                                    Message::EndArena(winning_team).ser(),
                                ))
                                .expect("This should send");
                        }
                        *game_in_progress = false;
                        println!("Game ended");
                        teams.clear();
                    }
                }
            }
        }

        lobbies.retain(|_, lobby| lobby.players.len() > 0);
    }

    Ok(())
}

fn main() {
    server();
}
