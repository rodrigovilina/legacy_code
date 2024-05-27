use std::io::{Write, Read};

use game::clear_terminal;
use sdl2::{mouse::MouseState, keyboard::KeyboardState};

use {connection::Connection, game::Game, window::Window};

mod connection;
mod fps;
mod keyboard;
mod loggable;
mod logger;
mod mouse;
mod window;

use {
  fps::FPS,
  logger::Logger,
  once_cell::sync::Lazy,
  sdl2::{pixels::Color, rect::Rect},
  std::sync::Mutex,
};

pub const FRAMERATE: f64 = 50.0;
pub static LOGGER: Lazy<Mutex<Logger>> =
  Lazy::new(|| Mutex::new(Logger::new("output.log").unwrap()));

pub struct Client {
  connection: Connection,
  window: Window,
  // game: Game,
  break_loop: bool,
}

type StringResult<T> = Result<T, String>;
type IO = Result<(), String>;

impl Client {
  fn new() -> Self {
    let connection: Connection = Connection::new().unwrap();
    let window: Window = Window::new().unwrap();

    Self {
      break_loop: false,
      connection,
      window,
    }
  }

  fn run(&mut self) -> IO {
    self.clear_and_present();

    FPS::new().fps(|frame| {
      //clear_terminal(); // this should actually be LOGGER.log_message("----------")
      self.window.update_title(frame)?;

      let mouse_state: MouseState = self.window.event_pump.mouse_state();
      self.window.mouse.update(mouse_state);

      let keyboard_state: KeyboardState = self.window.event_pump.keyboard_state();
      self.window.keyboard.update(keyboard_state);

      if frame == 100 {
        self.break_loop = true;
      }

      for _event in self.window.event_pump.poll_event() {}
      self.break_loop()?;

      self.write_to_server(frame);
      self.read_from_server(frame);

      self.clear_canvas();
      self.draw()?;
      self.window.canvas.present();
      Ok(())
    })
  }

  fn write_to_server(&mut self, frame: u128) -> () {
    let message: String = format!("F({})", frame);
    println!("Request: {} on F({})", message, frame);
    let message_bytes: &[u8] = message.as_bytes();
    self
      .connection
      .tcp_stream
      .write(message_bytes)
      .map_err(|e| e.to_string()).unwrap();
    self.connection.tcp_stream.flush().unwrap();
  }

  fn read_from_server(&mut self, frame: u128) -> () {
    let mut buffer = [0; 1024];
    self.connection.tcp_stream.read(&mut buffer).unwrap();
    println!("Response: {} on F({})", String::from_utf8_lossy(&buffer[..]), frame);
  }

  fn clear_and_present(&mut self) -> () {
    self.window.canvas.set_draw_color(Color::RGB(255, 0, 0));
    self.window.canvas.clear();
    self.window.canvas.present();
  }

  fn clear_canvas(&mut self) -> () {
    self.window.clear_white();
  }

  fn draw(&mut self) -> IO {
    self.window.canvas.set_draw_color(Color::RGBA(0, 0, 0, 255));
    self.window.canvas.fill_rect(Rect::new(0, 0, 20, 20))?;
    Ok(())
  }

  fn break_loop(&mut self) -> Result<(), String> {
    match self.break_loop {
      true => Err("".to_owned()),
      false => Ok(()),
    }
  }
}

fn main() -> IO {
  Client::new().run()
}
