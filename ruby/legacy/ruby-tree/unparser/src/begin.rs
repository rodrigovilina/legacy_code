use ast::{Begin, BeginStatement};

use crate::Unparse;

impl Unparse for Begin {
  fn unparse(&self) -> String {
    let body_text: Vec<String> = self.statements.iter().map(|node| node.unparse()).collect();
    format!("{}\n", body_text.join("\n"))
  }
}

impl Unparse for BeginStatement {
  fn unparse(&self) -> String {
    match self {
      BeginStatement::Alias(alias) => alias.unparse(),
      BeginStatement::True(true_) => true_.unparse(),
    }
  }
}

#[cfg(test)]
mod test {
  use {
    super::*,
    ast::{begin::s_begin, s, true_::True},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!("begin", s!(true), s!(true)).unparse(), "true\ntrue\n");
  }
}
