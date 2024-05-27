use super::{game_input::GameInput, Backend};

impl Backend {
  pub fn tick(self, game_input: GameInput) -> Self {
    self.update_player(&game_input)
  }

  fn update_player(self, game_input: &GameInput) -> Self {
    Self {
      player: self.player.update(game_input),
      ..self
    }
  }
}
