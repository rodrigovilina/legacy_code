mod acceleration;
pub mod game_input;
mod map;
mod player;
pub mod position;
pub mod screen;
pub mod size;
mod tick;
mod velocity;

use {
  self::{map::Map, player::Player, screen::Screen},
  crate::{server, SERVER},
};

#[derive(Clone)]
pub struct Backend {
  map: Map,
  player: Player,
}

impl Backend {
  pub fn new(player_id: usize) -> Self {
    let player: server::player::Player = SERVER.lock().unwrap().get_player(player_id);
    let map: server::map::Map = SERVER.lock().unwrap().get_map(player.map_id());

    Self {
      player: player.into(),
      map: map.into(),
    }
  }
}

impl Backend {
  pub fn present(&self) -> Screen {
    Screen::new(self.player.clone(), self.map.clone())
  }
}
