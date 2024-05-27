use crate::Node;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dstr {
  pub name: String,
}

impl Dstr {
  #[must_use]
  pub fn new(name: &str) -> Self {
    Self { name: name.to_string() }
  }

  #[must_use]
  pub fn node(name: &str) -> Node {
    Node::Dstr(Self::new(name))
  }
}

impl Default for Dstr {
  fn default() -> Self {
    Self::new("\"foo\"")
  }
}

impl From<Dstr> for Node {
  fn from(dstr: Dstr) -> Self {
    Dstr::node(&dstr.name)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_false_new() {
    assert_eq!(Dstr::new("\"foo\""), Dstr { name: "\"foo\"".to_string() });
  }

  #[test]
  fn test_false_node() {
    assert_eq!(Dstr::node("\"foo\""), Node::Dstr(Dstr::new("\"foo\"")));
  }

  #[test]
  fn test_false_default() {
    assert_eq!(Dstr::default(), Dstr::new("\"foo\""));
  }

  #[test]
  fn test_false_from() {
    assert_eq!(Node::from(Dstr::new("\"foo\"")), Dstr::node("\"foo\""));
  }
}
