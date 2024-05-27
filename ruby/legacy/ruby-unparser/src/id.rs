use crate::{Node, NodeBox};

pub fn unparse_id(node: &Node) -> String {
  if let Node::Id { name } = node {
    format!("{}", name)
  } else {
    panic!("Expected Node::Id, got {:?}", node);
  }
}

pub fn s_id(name: &str) -> NodeBox { Box::new(Node::Id { name: name.to_string() }) }

#[cfg(test)]
mod test {
  use {super::*, crate::Unparseable};

  #[test]
  fn test_unparse() {
    assert_eq!(Node::Id { name: "foo".to_string() }.unparse(), "foo");
  }
}
