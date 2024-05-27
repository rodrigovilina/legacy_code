#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Vertical {
  Up,
  Down,
  None,
}

impl Default for Vertical {
  fn default() -> Self {
    Self::None
  }
}

pub trait HasVertical {
  fn has_vertical(&self) -> Vertical;
}
