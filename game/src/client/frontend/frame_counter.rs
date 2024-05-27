use super::Frontend;

const DEBUG: bool = false;

impl Frontend {
  pub fn increase_frame_counter(&mut self) {
    self.frame += 1;
    self.debug_frame_counter()
  }

  fn debug_frame_counter(&mut self) {
    if DEBUG {
      println!("Frame: {:?}", self.frame);
    }
  }
}
