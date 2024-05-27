use eyro_values::player_id::PlayerId;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct PlayerBulletPacket {
  x: usize,
  y: usize,
  player_id: PlayerId,
}

impl PlayerBulletPacket {
  pub fn new(x: usize, y: usize, player_id: PlayerId) -> Self {
    Self { x, y, player_id }
  }

  pub fn x(&self) -> usize {
    self.x
  }

  pub fn y(&self) -> usize {
    self.y
  }

  pub fn player_id(&self) -> PlayerId {
    self.player_id
  }
}
