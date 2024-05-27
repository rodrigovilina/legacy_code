use crate::Node;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LVar {
  pub lexeme: String,
}

impl LVar {
  #[must_use]
  pub fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  #[must_use]
  pub fn node(lexeme: &str) -> Node {
    Node::LVar(Self::new(lexeme))
  }
}

impl Default for LVar {
  fn default() -> Self {
    Self::new("@foo")
  }
}

impl From<LVar> for Node {
  fn from(lvar: LVar) -> Self {
    LVar::node(&lvar.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_false_new() {
    assert_eq!(LVar::new("@foo"), LVar { lexeme: "@foo".to_string() });
  }

  #[test]
  fn test_false_node() {
    assert_eq!(LVar::node("@foo"), Node::LVar(LVar::new("@foo")));
  }

  #[test]
  fn test_false_default() {
    assert_eq!(LVar::default(), LVar::new("@foo"));
  }

  #[test]
  fn test_false_from() {
    assert_eq!(Node::from(LVar::new("@foo")), LVar::node("@foo"));
  }
}
