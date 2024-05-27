use crate::{Begin, Constant, Def};

pub fn s_def(name: &str, tipe: Option<Constant>, body: Begin) -> Def {
  Def { name: name.to_string(), tipe, body }
}

#[cfg(test)]
mod test {
  use {
    super::*,
    crate::{begin::s_begin, BeginStatement},
  };

  #[test]
  fn test_s_def() {
    let lines: Vec<BeginStatement> = vec![];
    let lines2: Vec<BeginStatement> = vec![];
    assert_eq!(
      s_def("foo", None, s_begin(lines)),
      Def { name: "foo".to_string(), tipe: None, body: s_begin(lines2) }
    )
  }
}
