use crate::{Node, NodeBox, Unparseable, indent::indent};

pub fn unparse_def(node: &Node) -> String {
  if let Node::Def { name, body } = node {
    let mut result: String = String::new();
    result.push_str("def ");
    result.push_str(&name.unparse());
    result.push_str("\n");
    let body_node = body.as_ref();
    if let Node::Begin { body } = body_node {
      if body.len() > 0 {
        result.push_str(&indent(&body_node.unparse()));
      }
    }
    result.push_str("end\n");
    result
  } else {
    panic!("Expected Node::Def, got {:?}", node);
  }
}

pub fn s_def(name: NodeBox, body: NodeBox) -> NodeBox {
  if let Node::Id { .. } = *name {
    Box::new(Node::Def { name, body })
  } else {
    panic!("Expected Node::Id, got {:?}", name);
  }
}

#[cfg(test)]
mod tests {
  use crate::{begin::s_begin, _true::s_true};

  use {super::*, crate::id::s_id};

  #[test]
  fn test_unparse() {
    assert_eq!(s_def(s_id("foo"), s_begin(vec![])).unparse(), "def foo\nend\n",);
    assert_eq!(
      s_def(
        s_id("foo"),
        s_begin(vec![s_true()])
      ).unparse(),
      "def foo\n  true\nend\n"
    );
  }
}
