#[allow(unused_macros)]
#[macro_export]
macro_rules! s {
  (begin) => {
    Begin::new(vec![])
  };
  (class $s:expr, nil) => {
    Class::new($s)
  };
  (comment $s:literal) => {
    Comment::new($s)
  };
  (const $s:literal) => {
    Const::new($s)
  };
  (dstr, $s:literal) => {
    Dstr::new($s)
  };
  (false) => {
    False::new()
  };
  (global $s:literal) => {
    Global::new($s)
  };
  (id, $s:literal) => {
    Id::new($s)
  };
  (int, $s:literal) => {
    Int::new($s)
  };
  (lvar $s:literal) => {
    LVar::new($s)
  };
  (module $s:expr, nil) => {
    Module::new($s)
  };
  (nil) => {
    Nil::new()
  };
  (self) => {
    Self_::new()
  };
  (sym, $s:literal) => {
    Symbol::new($s)
  };
  (send, nil, $s:literal) => {
    Send::new($s, None::<Id>)
  };
  (send, nil, $s:literal, $t:expr) => {
    Send::new($s, Some($t))
  };
  (true) => {
    True::new()
  };
}

#[cfg(test)]
mod tests {
  use crate::{
    begin::Begin, false_::False, nil::Nil, self_::Self_, send::Send,
    true_::True, Id, Symbol,
  };

  #[test]
  fn test_s() {
    assert_eq!(s!(begin), Begin::new(vec![]));
    assert_eq!(s!(false), False {});
    assert_eq!(s!(id, "foo"), Id::new("foo"));
    assert_eq!(s!(nil), Nil {});
    assert_eq!(s!(self), Self_ {});
    assert_eq!(s!(send, nil, "puts"), Send::new("puts", None::<Id>));
    assert_eq!(s!(sym, ":foo"), Symbol::new(":foo"));
    assert_eq!(s!(true), True {});
  }
}
