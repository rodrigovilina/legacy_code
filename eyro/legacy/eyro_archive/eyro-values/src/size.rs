#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Size {
  pub width: u64,
  pub height: u64,
}

pub trait Sizeable {
  fn size(&self) -> Size;

  fn width(&self) -> u64 {
    self.size().width()
  }

  fn height(&self) -> u64 {
    self.size().height()
  }
}

pub trait SizeableMut {
  fn set_size(&mut self, _size: Size);
}

impl Size {
  pub const fn new(width: u64, height: u64) -> Self {
    Self { width, height }
  }

  pub fn width(&self) -> u64 {
    self.width
  }

  pub fn height(&self) -> u64 {
    self.height
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  struct TestSizeable {
    size: Size,
  }

  impl Sizeable for TestSizeable {
    fn size(&self) -> Size {
      self.size
    }
  }

  #[test]
  fn test_size() {
    let size = Size::new(10, 20);
    assert_eq!(size.width(), 10);
    assert_eq!(size.height(), 20);
  }

  #[test]
  fn test_sizeable() {
    let sizeable = TestSizeable {
      size: Size::new(10, 20),
    };
    assert_eq!(sizeable.width(), 10);
    assert_eq!(sizeable.height(), 20);
  }
}
