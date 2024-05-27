use crate::Node;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Int {
  pub name: String,
}

impl Int {
  #[must_use]
  pub fn new(name: &str) -> Self {
    Self { name: name.to_string() }
  }

  #[must_use]
  pub fn node(name: &str) -> Node {
    Node::Int(Self::new(name))
  }
}

impl Default for Int {
  fn default() -> Self {
    Self::new("1234567890")
  }
}

impl From<Int> for Node {
  fn from(int: Int) -> Self {
    Int::node(&int.name)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_false_new() {
    assert_eq!(Int::new("1234567890"), Int { name: "1234567890".to_string() });
  }

  #[test]
  fn test_false_node() {
    assert_eq!(Int::node("1234567890"), Node::Int(Int::new("1234567890")));
  }

  #[test]
  fn test_false_default() {
    assert_eq!(Int::default(), Int::new("1234567890"));
  }

  #[test]
  fn test_false_from() {
    assert_eq!(Node::from(Int::new("1234567890")), Int::node("1234567890"));
  }
}
