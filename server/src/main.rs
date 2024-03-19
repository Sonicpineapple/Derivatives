use bimap::BiMap;
use itertools::Itertools;
use laminar::{ErrorKind, Packet, Socket, SocketEvent};
use std::{collections::HashMap, net::SocketAddr, thread};

use derivatives_core::{Message, SnakeTeam};

// for localhost
// const SERVER: &str = "127.0.0.1:12345";
// for webhost
const SERVER: &str = "0.0.0.0:12345";

fn server() -> Result<(), ErrorKind> {
    let mut socket = Socket::bind(SERVER)?;
    let (sender, receiver) = (socket.get_packet_sender(), socket.get_event_receiver());
    let _thread = thread::spawn(move || socket.start_polling());
    let mut next_id = 0;
    let mut clients: BiMap<u8, SocketAddr> = BiMap::new();
    let mut teams: HashMap<u8, SnakeTeam> = HashMap::new();
    let mut game_in_progress = false;
    const WIN_MARGIN: std::time::Duration = std::time::Duration::new(2, 0);
    let mut last_loss: Option<std::time::Instant> = None;

    loop {
        let rec = receiver.recv();
        if let Ok(event) = rec {
            match event {
                SocketEvent::Packet(packet) => {
                    if let Ok(msg) = Message::deser(packet.payload()) {
                        match msg {
                            Message::Connect => {
                                sender
                                    .send(Packet::reliable_unordered(
                                        packet.addr(),
                                        Message::Id(next_id).ser(),
                                    ))
                                    .expect("This should send");
                                println!("Assigned id {}", next_id);
                                clients.insert(next_id, packet.addr());
                                for (&id, &addr) in &clients {
                                    if addr != packet.addr() {
                                        sender
                                            .send(Packet::reliable_unordered(
                                                addr,
                                                Message::Join(next_id).ser(),
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
                                if game_in_progress {
                                    teams.insert(next_id, SnakeTeam::Spectator);
                                    println!("Id {} joined team {}", next_id, 0);
                                    sender
                                        .send(Packet::reliable_unordered(
                                            packet.addr(),
                                            Message::StartArena.ser(),
                                        ))
                                        .expect("This should send");
                                }
                                next_id += 1;
                            }
                            Message::Snake(_) => {
                                if clients.contains_right(&packet.addr()) {
                                    for &addr in clients.right_values() {
                                        if addr != packet.addr() {
                                            sender
                                                .send(Packet::reliable_unordered(addr, msg.ser()))
                                                .expect("This should send");
                                        }
                                    }
                                }
                            }
                            Message::Disconnect => {
                                let &leave_id = clients
                                    .get_by_right(&packet.addr())
                                    .expect("Address not assigned id");
                                println!("Client disconnected: {}, id {}", packet.addr(), leave_id);
                                clients.retain(|_, &addr| addr != packet.addr());
                                teams.retain(|&id, _| id != leave_id);
                                for &addr in clients.right_values() {
                                    sender
                                        .send(Packet::reliable_unordered(
                                            addr,
                                            Message::Leave(leave_id).ser(),
                                        ))
                                        .expect("This should send");
                                }
                            }
                            Message::Heartbeat => sender
                                .send(Packet::reliable_unordered(
                                    packet.addr(),
                                    Message::Heartbeat.ser(),
                                ))
                                .expect("This should send"),
                            Message::RegisterTeam(team_id) => {
                                let &id = clients.get_by_right(&packet.addr()).expect("No id");
                                if game_in_progress {
                                    last_loss = Some(std::time::Instant::now());
                                }
                                teams.insert(id, team_id);
                                println!("Id {} joined team {}", id, team_id)
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
                        teams.retain(|&id, _| id != leave_id);
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

        let team_players_left: Vec<&SnakeTeam> = teams
            .values()
            .filter(|&&team_id| team_id != SnakeTeam::Spectator)
            .collect();

        if !game_in_progress
            && clients.left_values().all(|id| teams.contains_key(id))
            && !team_players_left.iter().all_equal()
        {
            for &addr in clients.right_values() {
                sender
                    .send(Packet::reliable_unordered(addr, Message::StartArena.ser()))
                    .expect("This should send");
            }
            game_in_progress = true;
            println!("Game started");
        } else if game_in_progress && team_players_left.iter().all_equal() {
            if let Some(last_loss) = last_loss {
                if std::time::Instant::now().duration_since(last_loss) > WIN_MARGIN {
                    let winning_team = if let Some(&&team) = team_players_left.first() {
                        team
                    } else {
                        SnakeTeam::Spectator
                    };
                    for &addr in clients.right_values() {
                        sender
                            .send(Packet::reliable_unordered(
                                addr,
                                Message::EndArena(winning_team).ser(),
                            ))
                            .expect("This should send");
                    }
                    game_in_progress = false;
                    println!("Game ended");
                    teams.clear();
                }
            }
        }
    }

    Ok(())
}

fn main() {
    server();
}
