use std::{env, fs, process};

use {parser::parse, unparser::unparse::Unparse};

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    eprintln!("Usage: {} <file>", args[0]);
    process::exit(1);
  }

  let filename = &args[1];
  match read_and_print_file(filename) {
    Ok(()) => {}
    Err(e) => {
      eprintln!("Error reading file: {e}");
      process::exit(1);
    }
  }
}

fn read_and_print_file(filename: &str) -> Result<(), std::io::Error> {
  let content = fs::read_to_string(filename)?;
  let content = parse(&content).unparse();
  println!("{content}");
  Ok(())
}
