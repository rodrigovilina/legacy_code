use nonempty::NonEmpty;

use crate::{Node, NodeBox};

pub fn unparse_constant(node: &Node) -> String {
  if let Node::Constant { top_level, name } = node {
    let top_level: String = match top_level {
      true => "::".to_string(),
      false => String::new(),
    };
    let name: Vec<String> = name.clone().into();
    format!("{}{}", top_level, name.join("::"))
  } else {
    panic!("Expected Node::Node::Constant, got {:?}", node);
  }
}

pub fn s_const(top_level: bool, name: NonEmpty<String>) -> NodeBox {
  Box::new(Node::Constant { top_level, name: name.into() })
}

#[cfg(test)]
mod test {
  use {super::*, crate::Unparseable, nonempty::nonempty};

  #[test]
  fn test_unparse() {
    assert_eq!(
      Node::Constant { top_level: false, name: nonempty!("Foo".to_string()) }.unparse(),
      "Foo"
    );
    assert_eq!(
      Node::Constant { top_level: false, name: nonempty!("Foo".to_string(), "Bar".to_string()) }
        .unparse(),
      "Foo::Bar"
    );
    assert_eq!(
      Node::Constant { top_level: true, name: nonempty!("Foo".to_string()) }.unparse(),
      "::Foo"
    );
    assert_eq!(
      Node::Constant { top_level: true, name: nonempty!("Foo".to_string(), "Bar".to_string()) }
        .unparse(),
      "::Foo::Bar"
    );
  }
}
