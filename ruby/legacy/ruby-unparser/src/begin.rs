use crate::{Node, NodeBox, Unparseable};

pub fn unparse_begin(node: &Node) -> String {
  if let Node::Begin { body } = node {
    let body_text: Vec<String> = body.iter().map(|node| node.unparse()).collect();
    format!("{}\n", body_text.join("\n"))
  } else {
    panic!("Expected Node::Begin, got {:?}", node);
  }
}

pub fn s_begin(body: Vec<NodeBox>) -> NodeBox { Box::new(Node::Begin { body }) }

#[cfg(test)]
mod test {
  use {
    super::*,
    crate::{Unparseable, _true::s_true},
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s_begin(vec![s_true(), s_true(),]).unparse(), "true\ntrue\n");
  }
}
