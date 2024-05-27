// use ast::Def;
//
// pub fn unparse(node: Def) -> String {
//   let mut result = String::new();
//   result.push_str("def ");
//   result.push_str(&node.name);
//   if let Some(_tipe) = node.tipe {
//     result.push_str(" -> ");
//     result.push_str("String");
//   }
//   result.push_str("\n");
//
//   result
// }
//
// #[cfg(test)]
// #[allow(unused_imports)]
// mod tests {
//   use {
//     super::*,
//     ast::{begin::s_begin, constant::s_const, def::s_def, s, true_::s_true},
//     nonempty::nonempty,
//   };
//
//   #[test]
//   fn test_unparse() {
//     assert_eq!(
//       unparse(s!("def", "foo", Some(s!("const", false, "String")), s!("begin", s!(true)))),
//       "def foo -> String\n  true\nend\n".to_string()
//     )
//   }
// }
//
