use {crate::logger::Logger, once_cell::sync::Lazy, std::sync::Mutex};

pub const EXIT_FRAME: u128 = 1000;
pub const FRAMERATE: f64 = 100.0;
pub const LOG_TO_FILE: bool = false;
pub const LOG_TO_SCREEN: bool = true;
pub static LOGGER: Lazy<Mutex<Logger>> =
  Lazy::new(|| Mutex::new(Logger::new("output.log").unwrap()));
