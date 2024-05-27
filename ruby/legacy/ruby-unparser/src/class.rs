use crate::{Node, NodeBox, Unparseable};

pub fn unparse_class(node: &Node) -> String {
  if let Node::Class { name } = node {
    format!("class {}\nend\n", name.unparse())
  } else {
    panic!("Expected Node::Class, got {:?}", node);
  }
}

pub fn unparse_subclass(node: &Node) -> String {
  if let Node::SubClass { name, superclass } = node {
    format!("class {} < {}\nend\n", name.unparse(), superclass.unparse())
  } else {
    panic!("Expected Node::SubClass, got {:?}", node);
  }
}

pub fn s_class(name: NodeBox) -> NodeBox {
  if let Node::Constant { .. } = *name {
    Box::new(Node::Class { name })
  } else {
    panic!("Expected Node::Constant, got {:?}", name);
  }
}

pub fn s_subclass(name: NodeBox, superclass: NodeBox) -> NodeBox {
  if let Node::Constant { .. } = *name {
    if let Node::Constant { .. } = *superclass {
      Box::new(Node::SubClass { name, superclass })
    } else {
      panic!("Expected Node::Constant, got {:?}", superclass);
    }
  } else {
    panic!("Expected Node::Constant, got {:?}", name);
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    crate::{constant::s_const, Unparseable},
    nonempty::nonempty,
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s_class(s_const(false, nonempty!["Foo".to_string()])).unparse(), "class Foo\nend\n",);
    assert_eq!(
      s_class(s_const(false, nonempty!["Foo".to_string(), "Bar".to_string()])).unparse(),
      "class Foo::Bar\nend\n",
    );
    assert_eq!(
      s_class(s_const(true, nonempty!["Foo".to_string()])).unparse(),
      "class ::Foo\nend\n",
    );
    assert_eq!(
      s_subclass(
        s_const(false, nonempty!["Foo".to_string()]),
        s_const(false, nonempty!["Bar".to_string()]),
      )
      .unparse(),
      "class Foo < Bar\nend\n",
    );
  }
}
