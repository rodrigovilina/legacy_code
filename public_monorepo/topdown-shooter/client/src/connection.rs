use std::net::TcpStream;

pub struct Connection {
  pub tcp_stream: TcpStream,
}

impl Connection {
  pub fn new() -> Result<Self, String> {
    let tcp_stream = TcpStream::connect("127.0.0.1:8080").expect("Couldn't establish connection");

    Ok(Self { tcp_stream })
  }
}
