use super::{map::Map, player::Player};

pub enum Screen {
  Empty,
  Main { map: Map, player: Player },
}

impl Screen {
  pub fn new(player: Player, map: Map) -> Self {
    Self::Main { player, map }
  }
}

impl Default for Screen {
  fn default() -> Self {
    Self::Empty
  }
}
