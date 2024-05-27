use std::{
  io::{Read, Write},
  net::{TcpListener, TcpStream},
};

fn main() {
  let listener = TcpListener::bind("127.0.0.1:8080").expect("tcp listener error");

  for stream in listener.incoming() {
    let stream = stream.expect("");

    handle_connection(stream)
  }
}

fn handle_connection(mut stream: TcpStream) {
  println!("new client");
  let mut buffer = [0; 1024];

  loop {
    stream.read(&mut buffer).unwrap();
    println!("Request: {}", String::from_utf8_lossy(&buffer[..]));

    let response = "the response";

    stream.write(response.as_bytes()).unwrap();
    stream.flush().unwrap();
  }

  // stream.shutdown(Shutdown::Both).expect("shutdown call failed");
}
