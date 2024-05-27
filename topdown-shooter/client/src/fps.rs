use {
  crate::FRAMERATE,
  std::time::{Duration, Instant},
};

pub struct FPS {
  next_frame: Instant,
  frame: u128,
}

impl FPS {
  pub fn new() -> Self {
    let frame: u128 = 0;
    let next_frame: Instant = Instant::now();

    Self { next_frame, frame }
  }

  pub fn fps<Block>(&mut self, mut block: Block) -> Result<(), String>
  where
    Block: FnMut(u128) -> Result<(), String>,
  {
    let frame_time = FPS::frame_time();
    self.next_frame = Instant::now() + frame_time;

    'fps_loop: loop {
      let result = block(self.frame);

      match result {
        Ok(_) => (),
        Err(_) => break 'fps_loop,
      }

      self.frame += 1;
      self.next_frame += frame_time;

      let sleep_duration: Option<Duration> = self.next_frame.checked_duration_since(Instant::now());
      let Some(duration): Option<Duration> = sleep_duration else { continue };
      std::thread::sleep(duration);
    }

    Ok(())
  }

  fn frame_time() -> Duration {
    Duration::from_secs_f64(1.0 / FRAMERATE as f64)
  }
}
