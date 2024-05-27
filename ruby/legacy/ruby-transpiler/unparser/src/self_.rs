use {crate::unparse::Unparse, ast::Self_};

impl Unparse for Self_ {
  fn unparse(&self) -> String {
    "self".to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{s, self_::Self_},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(self).unparse(), "self");
  }
}
