use {crate::unparse::Unparse, ast::Symbol};

impl Unparse for Symbol {
  fn unparse(&self) -> String {
    self.lexeme.to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{s, symbol::Symbol},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(sym, ":foo").unparse(), ":foo");
  }
}
