use crate::Node;

pub fn unparse_false(node: &Node) -> String {
  if let Node::False = node {
    "false".to_string()
  } else {
    panic!("Expected Node::False, got {:?}", node);
  }
}

#[cfg(test)]
mod tests {
  use {super::*, crate::Unparseable};

  #[test]
  fn test_unparse() {
    assert_eq!(Node::False.unparse(), "false");
  }
}
