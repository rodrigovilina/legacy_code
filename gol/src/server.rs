#[derive(Clone, Debug)]
pub struct Server {
  exit_game: bool,
}

impl Server {
  pub fn new() -> Self { Self { exit_game: false } }

  pub fn exit_game(&self) -> bool { self.exit_game }

  pub fn set_exit_game(self, exit_game: bool) -> Self {
    Self { exit_game, ..self.clone() }
  }

  pub fn process_events(self, events: Vec<Event>) -> Self {
    // let mut exit_game: bool = self.exit_game;
    // for event in events {
    //   match event {
    //     Event::Quit { .. }
    //     | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
    //       exit_game = true;
    //     }
    //     _ => {}
    //   }
    // }
    // self.set_exit_game(exit_game)
  // }
}
