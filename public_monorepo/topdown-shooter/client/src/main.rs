mod config;
mod connection;
mod fps;
mod keyboard;
mod loggable;
mod logger;
mod mouse;
mod window;

use {
  config::EXIT_FRAME,
  connection::Connection,
  fps::FPS,
  game::{Game, MoveUnitEvent},
  sdl2::{pixels::Color, rect::Rect},
  std::io::{Read, Write},
  window::Window,
};

pub struct Client {
  connection: Connection,
  window: Window,
  game: Game,
  break_loop: bool,
}

type StringResult<T> = Result<T, String>;
type IO = StringResult<()>;

impl Client {
  fn new() -> Self {
    let connection: Connection = Connection::new().unwrap();
    let game: Game = Game::default();
    let window: Window = Window::new().unwrap();

    Self {
      break_loop: false,
      connection,
      game,
      window,
    }
  }

  fn run(&mut self) -> IO { // Result<nil, String>
    let mut message: String = String::new();
    self.clear_and_present();

    FPS::new().fps(|frame| {
      //clear_terminal(); // this should actually be LOGGER.log_message("----------")
      self.window.update_title(frame)?;
      self.window.update_mouse();
      self.window.update_keyboard();

      if frame == EXIT_FRAME {
        self.break_loop = true;
      }

      for event in self.window.event_pump.poll_event() {}

      self.break_loop()?;

      let move_unit_event: MoveUnitEvent = self.window.keyboard.event();

      message = String::new();
      message = format!("{}, {:?}", frame, move_unit_event);

      self.write_to_server(message.clone());
      self.read_from_server(frame);

      self.game.move_unit(move_unit_event);

      self.clear_canvas();
      self.draw()?;
      self.window.canvas.present();
      Ok(())
    })
  }

  fn write_to_server(&mut self, message: String) -> () {
    println!("Request: {}", message);
    let message_bytes: &[u8] = message.as_bytes();
    self
      .connection
      .tcp_stream
      .write(message_bytes)
      .map_err(|e| e.to_string())
      .unwrap();
    self.connection.tcp_stream.flush().unwrap();
  }

  fn read_from_server(&mut self, frame: u128) -> () {
    let mut buffer = [0; 1024];
    self.connection.tcp_stream.read(&mut buffer).unwrap();
    println!(
      "Response: {} on F({})",
      String::from_utf8_lossy(&buffer[..]),
      frame
    );
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
    self.window.canvas.fill_rect(Rect::new(
      self.game.unit.pos.0 as i32,
      self.game.unit.pos.1 as i32,
      20,
      20,
    ))?;
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
