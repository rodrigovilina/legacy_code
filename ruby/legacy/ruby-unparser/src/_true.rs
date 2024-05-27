use crate::{Node, NodeBox};

pub fn unparse_true(node: &Node) -> String {
  if let Node::True = node {
    "true".to_string()
  } else {
    panic!("Expected Node::True, got {:?}", node);
  }
}

pub fn s_true() -> NodeBox { Box::new(Node::True) }

#[cfg(test)]
mod tests {
  use {super::*, crate::Unparseable};

  #[test]
  fn test_unparse() {
    assert_eq!(Node::True.unparse(), "true");
  }
}
