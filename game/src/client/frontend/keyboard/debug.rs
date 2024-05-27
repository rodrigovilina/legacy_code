use super::Keyboard;

const DEBUG: bool = false;

impl Keyboard {
  pub fn debug(&self) {
    if DEBUG {
      println!("prev keys: {:?}", self.prev_keys);
      println!("keys: {:?}", self.pressed_keys);
      println!("new keys: {:?}", self.new_keys);
      println!("old keys: {:?}", self.released_keys);
    }
  }
}
