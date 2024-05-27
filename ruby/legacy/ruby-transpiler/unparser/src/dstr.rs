use {crate::unparse::Unparse, ast::Dstr};

impl Unparse for Dstr {
  fn unparse(&self) -> String {
    self.name.to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{dstr::Dstr, s},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(dstr, "\"foo\"").unparse(), "\"foo\"");
  }
}
