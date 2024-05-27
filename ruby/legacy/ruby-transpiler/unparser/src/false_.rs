use {crate::unparse::Unparse, ast::False};

impl Unparse for False {
  fn unparse(&self) -> String {
    "false".to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{false_::False, s},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(false).unparse(), "false");
  }
}
