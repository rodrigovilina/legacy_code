use nonempty::NonEmpty;

use crate::Node;

#[derive(Debug, PartialEq)]
pub struct Constant {
  pub top_level: bool,
  pub name: NonEmpty<String>,
}

impl Constant {
  pub fn new(top_level: bool, name: NonEmpty<&str>) -> Self {
    Self { top_level, name: name.map(|s| s.to_string()) }
  }
}

impl From<Constant> for Node {
  fn from(constant: Constant) -> Self { Node::Constant(constant) }
}

#[cfg(test)]
mod test {
  use {super::*, nonempty::nonempty};

  #[test]
  fn test_s_const() {
    assert_eq!(
      Constant::new(true, nonempty!["Foo", "Bar"]),
      Constant { top_level: true, name: nonempty!["Foo".to_string(), "Bar".to_string()] }
    )
  }
}
