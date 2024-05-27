use crate::Node;

pub fn unparse_global_variable(node: &Node) -> String {
  if let Node::GlobalVariable { name } = node {
    format!("${}", name)
  } else {
    panic!("Expected Node::GlobalVariable, got {:?}", node);
  }
}

#[cfg(test)]
mod test {
  use {super::*, crate::Unparseable};

  #[test]
  fn test_unparse() {
    assert_eq!(Node::GlobalVariable { name: "foo".to_string() }.unparse(), "$foo");
  }
}
