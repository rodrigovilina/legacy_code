use {crate::unparse::Unparse, ast::LVar};

impl Unparse for LVar {
  fn unparse(&self) -> String {
    self.lexeme.to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{lvar::LVar, s},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(lvar "@foo").unparse(), "@foo");
  }
}
