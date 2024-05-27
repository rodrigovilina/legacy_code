use crate::Node;

pub fn unparse_self(node: &Node) -> String {
  if let Node::Zelf = node {
    "self".to_string()
  } else {
    panic!("Expected Node::Zelf, got {:?}", node);
  }
}

#[cfg(test)]
mod tests {
  use {super::*, crate::Unparseable};

  #[test]
  fn test_unparse() {
    assert_eq!(Node::Zelf.unparse(), "self");
  }
}
