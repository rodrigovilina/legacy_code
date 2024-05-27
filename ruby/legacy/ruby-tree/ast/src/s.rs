macro_rules! s {
  ("alias", $from:expr, $to:expr) => {
    s_alias($from, $to)
  };
  ("begin") => {
    s_begin(vec![])
  };
  ("begin", $s:expr) => {
    s_begin($s)
  };
  ("begin", $($s:expr),*) => {
    s_begin(vec![$($s),*])
  };
  (class, $name:expr) => {
    s_class($name, None, s_begin(Vec::<BeginStatement>::new()))
  };
  (class, $name:expr, $body:expr) => {
    s_class($name, None, $body)
  };
  (const, $s:literal, $($t:literal),+) => {
    Constant::new($s, nonempty![$($t),+])
  };
  ("def", $name:expr, $tipe:expr, $body:expr) => {
    s_def($name, $tipe, $body)
  };
}
