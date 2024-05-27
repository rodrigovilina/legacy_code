use crate::Node;

pub fn unparse_symbol(node: &Node) -> String {
  if let Node::Symbol { name } = node {
    format!(":{}", name)
  } else {
    panic!("Expected Node::Symbol, got {:?}", node);
  }
}

#[cfg(test)]
mod test {
  use {super::*, crate::Unparseable};

  #[test]
  fn test_unparse() {
    assert_eq!(Node::Symbol { name: "foo".to_string() }.unparse(), ":foo");
  }
}
