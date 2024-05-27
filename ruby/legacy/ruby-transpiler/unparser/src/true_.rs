use {crate::unparse::Unparse, ast::True};

impl Unparse for True {
  fn unparse(&self) -> String {
    "true".to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{s, true_::True},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(true).unparse(), "true");
  }
}
