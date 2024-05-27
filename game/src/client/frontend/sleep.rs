use {
  super::Frontend,
  std::{thread::sleep, time::Duration},
};

pub trait Sleep {
  fn sleep(&mut self);
}

impl Sleep for Frontend {
  fn sleep(&mut self) {
    sleep(Duration::from_millis(50));
  }
}
