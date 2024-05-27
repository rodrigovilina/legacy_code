use ast::Class;

use crate::Unparse;

impl Unparse for Class {
  fn unparse(&self) -> String {
    let mut result: String = "class ".to_string();
    result.push_str(&self.name.unparse());
    if let Some(superclass) = &self.superclass {
      result.push_str(" < ");
      result.push_str(&superclass.unparse());
    }
    result.push('\n');
    result.push_str("end\n");
    result
  }
}
#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{begin::s_begin, class::s_class, constant::Constant, s, BeginStatement},
    nonempty::nonempty,
  };

  #[test]
  fn test_unparse() {
    assert_eq!(s!(class, s!(const, false, "Foo")).unparse(), "class Foo\nend\n",);
  }
}
