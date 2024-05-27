use std::{
  fs::{File, OpenOptions},
  io::Write,
};

pub struct Logger {
  file: File,
}

impl Logger {
  pub fn new(filename: &str) -> Result<Logger, String> {
    let file = OpenOptions::new()
      .write(true)
      .create(true)
      .append(true)
      .open(filename)
      .map_err(|e| e.to_string())?;
    Ok(Logger { file })
  }

  pub fn log_message(&mut self, message: &str) -> Result<(), String> {
    writeln!(self.file, "{}", message).map_err(|e| e.to_string())
  }
}
