use crate::Node;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Const {
  pub name: String,
}

impl Const {
  #[must_use]
  pub fn new(name: &str) -> Self {
    Self { name: name.to_string() }
  }

  #[must_use]
  pub fn node(name: &str) -> Node {
    Node::Const(Self::new(name))
  }
}

impl Default for Const {
  fn default() -> Self {
    Self::new("Foo")
  }
}

impl From<Const> for Node {
  fn from(id: Const) -> Self {
    Const::node(&id.name)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_false_new() {
    assert_eq!(Const::new("Foo"), Const { name: "Foo".to_string() });
  }

  #[test]
  fn test_false_node() {
    assert_eq!(Const::node("Foo"), Node::Const(Const::new("Foo")));
  }

  #[test]
  fn test_false_default() {
    assert_eq!(Const::default(), Const::new("Foo"));
  }

  #[test]
  fn test_false_from() {
    assert_eq!(Node::from(Const::new("Foo")), Const::node("Foo"));
  }
}
