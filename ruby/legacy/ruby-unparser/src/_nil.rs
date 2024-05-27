use crate::Node;

pub fn unparse_nil(node: &Node) -> String {
  if let Node::Nil = node {
    "nil".to_string()
  } else {
    panic!("Expected Node::Nil, got {:?}", node);
  }
}

#[cfg(test)]
mod tests {
  use {super::*, crate::Unparseable};

  #[test]
  fn test_unparse() {
    assert_eq!(Node::Nil.unparse(), "nil");
  }
}
