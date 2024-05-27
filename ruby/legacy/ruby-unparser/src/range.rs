// use crate::Unparseable;
//
// pub enum Range {
//   BoundlessInclusive {},
//   BoundlessExclusive {},
//   BeginlessInclusive {
//     end: Box<dyn Unparseable>,
//   },
//   BeginlessExclusive {
//     end: Box<dyn Unparseable>,
//   },
//   EndlessInclusive {
//     start: Box<dyn Unparseable>,
//   },
//   EndlessExclusive {
//     start: Box<dyn Unparseable>,
//   },
//   Inclusive {
//     start: Box<dyn Unparseable>,
//     end: Box<dyn Unparseable>,
//   },
//   Exclusive {
//     start: Box<dyn Unparseable>,
//     end: Box<dyn Unparseable>,
//   },
// }
//
// impl Unparseable for Range {
//   fn unparse(&self) -> String {
//     match self {
//       Self::BoundlessInclusive {} => "(nil..)".to_string(),
//       Self::BoundlessExclusive {} => "(nil...)".to_string(),
//       Self::BeginlessInclusive { end } => format!("(..{})", end.unparse()),
//       Self::BeginlessExclusive { end } => format!("(...{})", end.unparse()),
//       Self::EndlessInclusive { start } => format!("({}..)", start.unparse()),
//       Self::EndlessExclusive { start } => format!("({}...)", start.unparse()),
//       Self::Inclusive { start, end } => {
//         format!("({}..{})", start.unparse(), end.unparse())
//       }
//       Self::Exclusive { start, end } => {
//         format!("({}...{})", start.unparse(), end.unparse())
//       }
//     }
//   }
// }
//
// #[cfg(test)]
// mod test {
//   use crate::_true::True;
//
//   use super::*;
//
//   #[test]
//   fn test_boundless_inclusive() {
//     let range: Range = Range::BoundlessInclusive {};
//     assert_eq!(range.unparse(), "(nil..)");
//   }
//
//   #[test]
//   fn test_boundless_exclusive() {
//     let range: Range = Range::BoundlessExclusive {};
//     assert_eq!(range.unparse(), "(nil...)");
//   }
//
//   #[test]
//   fn test_beginless_inclusive() {
//     let range: Range = Range::BeginlessInclusive {
//       end: Box::new(True {}),
//     };
//     assert_eq!(range.unparse(), "(..true)");
//   }
//
//   #[test]
//   fn test_beginless_exclusive() {
//     let range: Range = Range::BeginlessExclusive {
//       end: Box::new(True {}),
//     };
//     assert_eq!(range.unparse(), "(...true)");
//   }
//
//   #[test]
//   fn test_endless_inclusive() {
//     let range: Range = Range::EndlessInclusive {
//       start: Box::new(True {}),
//     };
//     assert_eq!(range.unparse(), "(true..)");
//   }
//
//   #[test]
//   fn test_endless_exclusive() {
//     let range: Range = Range::EndlessExclusive {
//       start: Box::new(True {}),
//     };
//     assert_eq!(range.unparse(), "(true...)");
//   }
//
//   #[test]
//   fn test_inclusive() {
//     let range: Range = Range::Inclusive {
//       start: Box::new(True {}),
//       end: Box::new(True {}),
//     };
//     assert_eq!(range.unparse(), "(true..true)");
//   }
//
//   #[test]
//   fn test_exclusive() {
//     let range: Range = Range::Exclusive {
//       start: Box::new(True {}),
//       end: Box::new(True {}),
//     };
//     assert_eq!(range.unparse(), "(true...true)");
//   }
// }
//
