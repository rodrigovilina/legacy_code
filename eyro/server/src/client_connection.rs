use std::net::{SocketAddr, TcpStream};

#[derive(Debug)]
pub struct Client {
  pub stream: TcpStream,
  pub ip: SocketAddr,
}
