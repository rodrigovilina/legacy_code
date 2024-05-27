#![warn(clippy::complexity)]
#![warn(clippy::expect_used)]
#![warn(clippy::nursery)]
#![warn(clippy::panic)]
#![warn(clippy::pedantic)]
#![warn(clippy::perf)]
#![warn(clippy::unwrap_used)]

use {
  core::packet::Packet,
  std::{
    io::{Read, Write},
    net::TcpStream,
    thread::sleep,
    time::Duration,
  },
};

struct Stage1 {
  stream: TcpStream,
}

struct Stage2 {
  stream: TcpStream,
  _client_id: u64,
}

struct Stage3 {
  stream: TcpStream,
  client_id: u64,
  characters: Vec<()>,
}

fn main() -> Result<(), String> {
  let stream = TcpStream::connect("127.0.0.1:8765").map_err(|e| e.to_string())?;
  let stage_1: Stage1 = Stage1 { stream };
  let mut stage_2: Stage2 = stage_1.get_client_id()?;
  stage_2.send_login();
  loop {
    let _ = read_packets(&mut stage_2.stream);
    send_healthcheck(&mut stage_2.stream);
    sleep(Duration::from_millis(1000));
  }
}

impl Stage1 {
  fn get_client_id(mut self) -> Result<Stage2, String> {
    let mut buf: [u8; 9] = [0; 9];
    self
      .stream
      .read_exact(&mut buf)
      .map_err(|e| e.to_string())?;

    let packet = Packet::from_bytes(&buf).ok_or("Unknown Packet")?;
    if let Packet::AcknowledgeConnection { client_id } = packet {
      Ok(Stage2 {
        stream: self.stream,
        _client_id: client_id,
      })
    } else {
      Err("Couldn't Get ClientId".to_string())
    }
  }
}

impl Stage2 {
  fn send_login(&mut self) {
    let packet = Packet::Login {
      username: String::from("asdf"),
      password: String::from("fdsa"),
    };
    let _ = self.stream.write(&packet.as_bytes());
  }
}

fn read_packets(stream: &mut TcpStream) -> Vec<Packet> {
  let packets: Vec<Packet> = Packet::read_multiple_from_stream(stream);
  packets
}

fn send_healthcheck(stream: &mut TcpStream) {
  let packet = Packet::HealthCheck;
  let _ = stream.write(&packet.as_bytes());
}
