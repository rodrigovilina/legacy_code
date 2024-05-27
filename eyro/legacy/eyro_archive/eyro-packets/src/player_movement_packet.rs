use eyro_values::{acceleration::Acceleration, horizontal::Horizontal, vertical::Vertical};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct PlayerMovementPacket {
  pub horizontal: Horizontal,
  pub vertical: Vertical,
}

impl PlayerMovementPacket {
  pub fn new(horizontal: Horizontal, vertical: Vertical) -> Self {
    Self {
      horizontal,
      vertical,
    }
  }

  pub fn horizontal(&self) -> Horizontal {
    self.horizontal
  }

  pub fn vertical(&self) -> Vertical {
    self.vertical
  }

  pub fn acc(&self) -> Acceleration {
    match (self.horizontal(), self.vertical()) {
      (Horizontal::Left, Vertical::Up) => Acceleration::cents(-70, -70),
      (Horizontal::Left, Vertical::Down) => Acceleration::cents(-70, 70),
      (Horizontal::Right, Vertical::Up) => Acceleration::cents(70, -70),
      (Horizontal::Right, Vertical::Down) => Acceleration::cents(70, 70),
      (Horizontal::Left, Vertical::None) => Acceleration::cents(-100, 0),
      (Horizontal::Right, Vertical::None) => Acceleration::cents(100, 0),
      (Horizontal::None, Vertical::Up) => Acceleration::cents(0, -100),
      (Horizontal::None, Vertical::Down) => Acceleration::cents(0, 100),
      (Horizontal::None, Vertical::None) => Acceleration::cents(0, 0),
    }
  }
}
