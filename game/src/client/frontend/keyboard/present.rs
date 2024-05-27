use {super::Keyboard, crate::client::backend::game_input::GameInput, sdl2::keyboard::Keycode};

impl Keyboard {
  pub fn present(&self) -> GameInput {
    GameInput {
      left: self.pressed_keys.contains(&Keycode::Left),
      right: self.pressed_keys.contains(&Keycode::Right),
      up: self.pressed_keys.contains(&Keycode::Up),
    }
  }
}
