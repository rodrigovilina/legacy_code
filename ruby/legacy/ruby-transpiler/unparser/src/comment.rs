use {crate::unparse::Unparse, ast::Comment};

impl Unparse for Comment {
  fn unparse(&self) -> String {
    self.lexeme.to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{comment::Comment, s},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(comment "# foo").unparse(), "# foo");
  }
}
