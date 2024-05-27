use crate::Node;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Begin {
  statements: Vec<Node>,
}

impl Begin {
  #[must_use]
  pub fn new(statements: Vec<Node>) -> Self {
    Self { statements }
  }

  #[must_use]
  pub fn node(&self) -> Node {
    Node::Begin(self.clone())
  }

  pub fn statements(&self) -> &Vec<Node> {
    &self.statements
  }
}

impl Default for Begin {
  fn default() -> Self {
    Self::new(vec![])
  }
}

impl From<Begin> for Node {
  fn from(begin: Begin) -> Self {
    begin.node()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_false_new() {
    assert_eq!(Begin::new(vec![]), Begin { statements: vec![] });
  }

  #[test]
  fn test_false_node() {
    assert_eq!(Begin::new(vec![]).node(), Node::Begin(Begin::new(vec![])));
  }

  #[test]
  fn test_false_default() {
    assert_eq!(Begin::default(), Begin::new(vec![]));
  }

  #[test]
  fn test_false_from() {
    assert_eq!(Node::from(Begin::new(vec![])), Node::Begin(Begin::new(vec![])));
  }
}
