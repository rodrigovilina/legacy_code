use {
  crate::{config::SLEEP_TIME, server::Server},
  std::{thread::sleep, time::Duration},
};

impl Server {
  pub fn sleep(&self) {
    sleep(Duration::from_millis(SLEEP_TIME));
  }
}
