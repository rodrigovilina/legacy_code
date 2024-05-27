use crate::{Const, Node};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Class {
  pub name: Const,
}

impl Class {
  #[must_use]
  pub fn new(const_: Const) -> Self {
    Self { name: const_ }
  }

  #[must_use]
  pub fn node(&self) -> Node {
    Node::Class(self.clone())
  }
}
//
//impl Default for Symbol {
//  fn default() -> Self {
//    Self::new(":foo")
//  }
//}
//
impl From<Class> for Node {
  fn from(class: Class) -> Self {
    class.node()
  }
}
//
//#[cfg(test)]
//mod tests {
//  use super::*;
//
//  #[test]
//  fn test_false_new() {
//    assert_eq!(Symbol::new(":foo"), Symbol { lexeme: ":foo".to_string() });
//  }
//
//  #[test]
//  fn test_false_node() {
//    assert_eq!(Symbol::node(":foo"), Node::Symbol(Symbol::new(":foo")));
//  }
//
//  #[test]
//  fn test_false_default() {
//    assert_eq!(Symbol::default(), Symbol::new(":foo"));
//  }
//
//  #[test]
//  fn test_false_from() {
//    assert_eq!(Node::from(Symbol::new(":foo")), Symbol::node(":foo"));
//  }
//}
//
