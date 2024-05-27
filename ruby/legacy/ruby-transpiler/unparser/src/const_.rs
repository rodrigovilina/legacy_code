use {crate::unparse::Unparse, ast::Const};

impl Unparse for Const {
  fn unparse(&self) -> String {
    self.name.to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{const_::Const, s},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(const "Foo").unparse(), "Foo");
  }
}
