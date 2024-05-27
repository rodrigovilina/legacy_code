use crate::{Const, Node};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
  pub name: Const,
}

impl Module {
  #[must_use]
  pub fn new(const_: Const) -> Self {
    Self { name: const_ }
  }

  #[must_use]
  pub fn node(&self) -> Node {
    Node::Module(self.clone())
  }
}
//
//impl Default for Symbol {
//  fn default() -> Self {
//    Self::new(":foo")
//  }
//}
//
impl From<Module> for Node {
  fn from(module: Module) -> Self {
    module.node()
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
