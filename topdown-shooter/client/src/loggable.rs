use super::LOGGER;

pub trait Loggable {
  fn log_message(&self) -> String;
  fn log(&self) -> Result<(), String> {
    LOGGER.lock().unwrap().log_message(&self.log_message())
  }
}
