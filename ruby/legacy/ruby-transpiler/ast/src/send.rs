use crate::{Dstr, Id, Node};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Send {
  pub method: String,
  pub argument: Option<SendArg>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SendArg {
  Id(Id),
  Dstr(Dstr),
}

impl From<Id> for SendArg {
  fn from(id: Id) -> Self {
    Self::Id(id)
  }
}

impl From<Dstr> for SendArg {
  fn from(dstr: Dstr) -> Self {
    Self::Dstr(dstr)
  }
}

impl Send {
  pub fn new(method: &str, argument: Option<impl Into<SendArg>>) -> Self {
    Self {
      method: method.to_string(),
      argument: argument.map(std::convert::Into::into),
    }
  }

  #[must_use]
  pub fn node(&self) -> Node {
    Node::Send(self.clone())
  }
}

impl Default for Send {
  fn default() -> Self {
    todo!()
  }
}

impl From<Send> for Node {
  fn from(send: Send) -> Self {
    send.node()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_send_new() {
    assert_eq!(
      Send::new("puts", None::<SendArg>),
      Send { method: "puts".to_string(), argument: None }
    );
    assert_eq!(
      Send::new("puts", Some(Id::new(""))),
      Send {
        method: "puts".to_string(),
        argument: Some(SendArg::Id(Id::new("")))
      }
    );
  }

  // #[test]
  // fn test_false_node() {
  //   assert_eq!(Begin::node(), Node::Begin(Begin {}));
  // }

  // #[test]
  // fn test_false_default() {
  //   assert_eq!(Begin::default(), Begin {});
  // }

  // #[test]
  // fn test_false_from() {
  //   assert_eq!(Node::from(Begin::new()), Node::Begin(Begin {}));
  // }
}
