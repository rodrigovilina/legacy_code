use {crate::unparse::Unparse, ast::Nil};

impl Unparse for Nil {
  fn unparse(&self) -> String {
    "nil".to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{nil::Nil, s},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(nil).unparse(), "nil");
  }
}
