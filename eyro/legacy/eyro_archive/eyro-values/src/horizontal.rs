#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Horizontal {
  Left,
  Right,
  None,
}

impl Default for Horizontal {
  fn default() -> Self {
    Self::None
  }
}

pub trait HasHorizontal {
  fn horizontal(&self) -> Horizontal;
}
