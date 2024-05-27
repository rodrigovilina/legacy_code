use {crate::unparse::Unparse, ast::Module};

impl Unparse for Module {
  fn unparse(&self) -> String {
    format!("module {}\nend", self.name.unparse())
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{const_::Const, module::Module, s},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(module s!(const "Foo"), nil).unparse(), "module Foo\nend");
  }
}
