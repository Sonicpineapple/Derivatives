use bimap::BiMap;
use laminar::{ErrorKind, Packet, Socket, SocketEvent};
use std::{net::SocketAddr, thread};

use derivatives_core::Message;

const SERVER: &str = "127.0.0.1:12345";

fn server() -> Result<(), ErrorKind> {
    let mut socket = Socket::bind(SERVER)?;
    let (sender, receiver) = (socket.get_packet_sender(), socket.get_event_receiver());
    let _thread = thread::spawn(move || socket.start_polling());
    let mut next_id = 0;
    let mut clients: BiMap<u8, SocketAddr> = BiMap::new();

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
                                next_id += 1;
                            }
                            Message::Snake(_) => {
                                for &addr in clients.right_values() {
                                    if addr != packet.addr() {
                                        sender
                                            .send(Packet::reliable_unordered(addr, msg.ser()))
                                            .expect("This should send");
                                    }
                                }
                            }
                            Message::Disconnect => {
                                let &leave_id = clients
                                    .get_by_right(&packet.addr())
                                    .expect("Address not assigned id");
                                println!("Client disconnected: {}, id {}", packet.addr(), leave_id);
                                clients.retain(|_, &addr| addr != packet.addr());
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
                            _ => todo!(),
                        }
                        //dbg!(std::time::Instant::now());
                    } else {
                        println!("Garbage message")
                    }
                }
                SocketEvent::Timeout(address) => {
                    let &leave_id = clients
                        .get_by_right(&address)
                        .expect("Address not assigned id");
                    println!("Client timed out: {}, id {}", address, leave_id);
                    clients.retain(|_, &addr| addr != address);
                    for &addr in clients.right_values() {
                        sender
                            .send(Packet::reliable_unordered(
                                addr,
                                Message::Leave(leave_id).ser(),
                            ))
                            .expect("This should send");
                    }
                }
                _ => {
                    dbg!(event);
                }
            }
        } else {
            dbg!(rec);
        }
    }

    Ok(())
}

fn main() {
    server();
}
