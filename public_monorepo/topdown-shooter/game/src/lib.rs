#[derive(Default, Debug)]
pub struct Unit {
  pub pos: (usize, usize),
}

#[derive(Default, Debug)]
pub struct Game {
  pub unit: Unit,
}

impl Game {
  pub fn move_unit(&mut self, event: MoveUnitEvent) -> () {
    match event {
      MoveUnitEvent::Down | MoveUnitEvent::DownLeft | MoveUnitEvent::DownRight => {
        self.unit.pos.1 += 1
      }
      MoveUnitEvent::Up | MoveUnitEvent::UpLeft | MoveUnitEvent::UpRight => {
        self.unit.pos.1 -= 1
      }
      _ => {}
    }

    match event {
      MoveUnitEvent::Left | MoveUnitEvent::DownLeft | MoveUnitEvent::UpLeft => {
        self.unit.pos.0 -= 1
      }
      MoveUnitEvent::Right | MoveUnitEvent::DownRight | MoveUnitEvent::UpRight => {
        self.unit.pos.0 += 1
      }
      _ => {}
    }
  }
}

pub fn clear_terminal() {
  print!("\x1B[2J\x1B[1;1H");
}

#[derive(Debug)]
pub enum MoveUnitEvent {
  None,
  Up,
  UpLeft,
  UpRight,
  Left,
  Right,
  Down,
  DownLeft,
  DownRight,
}

impl MoveUnitEvent {
  pub fn move_up(self) -> Self {
    match self {
      MoveUnitEvent::None => MoveUnitEvent::Up,
      MoveUnitEvent::Left => MoveUnitEvent::UpLeft,
      MoveUnitEvent::Right => MoveUnitEvent::UpRight,
      _ => self,
    }
  }

  pub fn move_left(self) -> Self {
    match self {
      MoveUnitEvent::None => MoveUnitEvent::Left,
      MoveUnitEvent::Up => MoveUnitEvent::UpLeft,
      MoveUnitEvent::Down => MoveUnitEvent::DownLeft,
      _ => self,
    }
  }

  pub fn move_down(self) -> Self {
    match self {
      MoveUnitEvent::None => MoveUnitEvent::Down,
      MoveUnitEvent::Left => MoveUnitEvent::DownLeft,
      MoveUnitEvent::Right => MoveUnitEvent::DownRight,
      _ => self,
    }
  }

  pub fn move_right(self) -> Self {
    match self {
      MoveUnitEvent::None => MoveUnitEvent::Right,
      MoveUnitEvent::Up => MoveUnitEvent::UpRight,
      MoveUnitEvent::Down => MoveUnitEvent::DownRight,
      _ => self,
    }
  }
}

impl Default for MoveUnitEvent {
  fn default() -> Self {
    Self::None
  }
}
