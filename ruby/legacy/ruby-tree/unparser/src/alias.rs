use ast::{Alias, AliasArg};

use crate::Unparse;

impl Unparse for Alias {
  fn unparse(&self) -> String { format!("alias {} {}", self.from.unparse(), self.to.unparse()) }
}

impl Unparse for AliasArg {
  fn unparse(&self) -> String {
    match self {
      AliasArg::Id(id) => id.unparse(),
      AliasArg::Global(global) => global.unparse(),
      AliasArg::Symbol(symbol) => symbol.unparse(),
    }
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{alias::s_alias, global::Global, id::Id, s, symbol::Symbol},
  };

  #[test]
  fn test_alias_with_ids() {
    assert_eq!(s!("alias", s!(id, "hello"), s!(id, "bye")).unparse(), "alias hello bye")
  }

  #[test]
  fn test_alias_with_global_variables() {
    assert_eq!(
      s!("alias", s!(global, "$hello"), s!(global, "$bye")).unparse(),
      "alias $hello $bye"
    )
  }

  #[test]
  fn test_alias_with_symbols() {
    assert_eq!(s!("alias", s!(symbol, ":hello"), s!(symbol, ":bye")).unparse(), "alias :hello :bye")
  }
}
