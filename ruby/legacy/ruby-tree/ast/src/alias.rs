use crate::{Alias, AliasArg, Global, Id, Symbol};

pub fn s_alias(from: impl Into<AliasArg> + Clone, to: impl Into<AliasArg> + Clone) -> Alias {
  match (from.clone().into(), to.clone().into()) {
    (AliasArg::Global(..), AliasArg::Global(..))
    | (AliasArg::Id(..), AliasArg::Id(..))
    | (AliasArg::Id(..), AliasArg::Symbol(..))
    | (AliasArg::Symbol(..), AliasArg::Id(..))
    | (AliasArg::Symbol(..), AliasArg::Symbol(..)) => Alias { from: from.into(), to: to.into() },
    _ => panic!("Invalid alias"),
  }
}

impl From<Global> for AliasArg {
  fn from(global: Global) -> Self { AliasArg::Global(global) }
}

impl From<Id> for AliasArg {
  fn from(id: Id) -> Self { AliasArg::Id(id) }
}

impl From<Symbol> for AliasArg {
  fn from(symbol: Symbol) -> Self { AliasArg::Symbol(symbol) }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    crate::{global::Global, s},
  };

  #[test]
  fn test_s_alias() {
    assert_eq!(
      s_alias(s!(global, "foo"), s!(global, "bar")),
      Alias { from: s!(global, "foo").into(), to: s!(global, "bar").into() }
    )
  }
}
