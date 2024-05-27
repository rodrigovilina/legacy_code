use crate::{Alias, Begin, BeginStatement, True};

pub fn s_begin(statements: Vec<impl Into<BeginStatement>>) -> Begin {
  let statements: Vec<BeginStatement> = statements.into_iter().map(|s| s.into()).collect();
  Begin { statements }
}

impl From<Alias> for BeginStatement {
  fn from(alias: Alias) -> Self { BeginStatement::Alias(alias) }
}

impl From<True> for BeginStatement {
  fn from(true_: True) -> Self { BeginStatement::True(true_) }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    crate::{alias::s_alias, global::Global, s},
  };

  #[test]
  fn test_s_begin() {
    assert_eq!(
      s_begin(vec![s_alias(s!(global, "foo"), s!(global, "bar"))]),
      Begin { statements: vec![s_alias(s!(global, "foo"), s!(global, "bar")).into()] }
    )
  }
}
