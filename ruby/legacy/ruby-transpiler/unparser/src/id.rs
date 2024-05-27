use {crate::unparse::Unparse, ast::Id};

impl Unparse for Id {
  fn unparse(&self) -> String {
    self.name.to_string()
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{id::Id, s},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(id, "foo").unparse(), "foo");
  }
}
