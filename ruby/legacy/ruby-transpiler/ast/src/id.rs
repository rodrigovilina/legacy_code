use crate::Node;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Id {
  pub name: String,
}

impl Id {
  #[must_use]
  pub fn new(name: &str) -> Self {
    Self { name: name.to_string() }
  }

  #[must_use]
  pub fn node(name: &str) -> Node {
    Node::Id(Self::new(name))
  }
}

impl Default for Id {
  fn default() -> Self {
    Self::new("foo")
  }
}

impl From<Id> for Node {
  fn from(id: Id) -> Self {
    Id::node(&id.name)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_false_new() {
    assert_eq!(Id::new("foo"), Id { name: "foo".to_string() });
  }

  #[test]
  fn test_false_node() {
    assert_eq!(Id::node("foo"), Node::Id(Id::new("foo")));
  }

  #[test]
  fn test_false_default() {
    assert_eq!(Id::default(), Id::new("foo"));
  }

  #[test]
  fn test_false_from() {
    assert_eq!(Node::from(Id::new("foo")), Id::node("foo"));
  }
}
