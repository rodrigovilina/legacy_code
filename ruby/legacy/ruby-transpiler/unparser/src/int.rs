use {crate::unparse::Unparse, ast::Int};

impl Unparse for Int {
  fn unparse(&self) -> String {
    self.name.to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{int::Int, s},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(int, "1234567890").unparse(), "1234567890");
  }
}
