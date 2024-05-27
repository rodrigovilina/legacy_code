use crate::Node;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Global {
  pub lexeme: String,
}

impl Global {
  #[must_use]
  pub fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  #[must_use]
  pub fn node(lexeme: &str) -> Node {
    Node::Global(Self::new(lexeme))
  }
}

impl Default for Global {
  fn default() -> Self {
    Self::new("$foo")
  }
}

impl From<Global> for Node {
  fn from(global: Global) -> Self {
    Global::node(&global.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_false_new() {
    assert_eq!(Global::new("$foo"), Global { lexeme: "$foo".to_string() });
  }

  #[test]
  fn test_false_node() {
    assert_eq!(Global::node("$foo"), Node::Global(Global::new("$foo")));
  }

  #[test]
  fn test_false_default() {
    assert_eq!(Global::default(), Global::new("$foo"));
  }

  #[test]
  fn test_false_from() {
    assert_eq!(Node::from(Global::new("$foo")), Global::node("$foo"));
  }
}
