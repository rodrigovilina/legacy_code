use crate::{Node, Unparseable};

pub fn unparse_alias(node: &Node) -> String {
  if let Node::Alias(from, to) = node {
    format!("alias {} {}", from.unparse(), to.unparse())
  } else {
    panic!("Expected Node::Alias, got {:?}", node);
  }
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    crate::{id::s_id, NodeBox, Unparseable},
  };

  fn s_alias(from: NodeBox, to: NodeBox) -> NodeBox { Box::new(Node::Alias(from, to)) }

  fn s_global(name: &str) -> NodeBox { Box::new(Node::GlobalVariable { name: name.to_string() }) }

  fn s_symbol(name: &str) -> NodeBox { Box::new(Node::Symbol { name: name.to_string() }) }

  #[test]
  fn test_alias_with_ids() {
    assert_eq!(s_alias(s_id("hello"), s_id("bye")).unparse(), "alias hello bye")
  }

  #[test]
  fn test_alias_with_global_variables() {
    assert_eq!(s_alias(s_global("hello"), s_global("bye")).unparse(), "alias $hello $bye")
  }

  #[test]
  fn test_alias_with_symbols() {
    assert_eq!(s_alias(s_symbol("hello"), s_symbol("bye")).unparse(), "alias :hello :bye")
  }
}
