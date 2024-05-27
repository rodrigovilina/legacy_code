use {crate::unparse::Unparse, ast::Begin};

impl Unparse for Begin {
  fn unparse(&self) -> String {
    self
      .statements()
      .iter()
      .map(|s| s.unparse())
      .collect::<Vec<String>>()
      .join("\n")
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{begin::Begin, s},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(begin).unparse(), "");
  }
}
