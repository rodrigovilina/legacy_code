pub fn indent(input: &str) -> String {
  if input == "" {
    return String::new();
  }
  let mut result = input
      .lines()
      .map(indent_line)
      .collect::<Vec<String>>()
      .join("\n");

  result.push_str("\n");
  result
}

pub fn indent_line(input: &str) -> String {
  if input == "" {
    return String::new();
  }
  format!("  {}", input)
}
