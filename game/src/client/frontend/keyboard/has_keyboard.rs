use crate::client::{backend::game_input::GameInput, frontend::Frontend};

pub trait HasKeyboard {
  fn update_keyboard(&mut self);

  fn debug_keyboard(&mut self);

  fn present_keyboard(&self) -> GameInput;
}

impl HasKeyboard for Frontend {
  fn update_keyboard(&mut self) {
    self.keyboard.update(&self.event_pump);
    self.debug_keyboard()
  }

  fn debug_keyboard(&mut self) {
    self.keyboard.debug();
  }

  fn present_keyboard(&self) -> GameInput {
    self.keyboard.present()
  }
}
