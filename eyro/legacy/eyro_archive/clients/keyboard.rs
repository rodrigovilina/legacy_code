use {
  sdl2::{keyboard::Keycode, EventPump},
  std::collections::HashSet,
};

pub struct Keyboard {
  pub pressed_keys: HashSet<Keycode>,
  // Keys that were pressed in the previous frame.
  prev_keys: HashSet<Keycode>,
  // Newly pressed keys in this frame.
  pub new_keys: HashSet<Keycode>,
  released_keys: HashSet<Keycode>,
}

impl Keyboard {
  pub fn new() -> Self {
    Self {
      pressed_keys: HashSet::new(),
      prev_keys: HashSet::new(),
      new_keys: HashSet::new(),
      released_keys: HashSet::new(),
    }
  }

  pub fn update(&mut self, event_pump: &EventPump) {
    self.prev_keys = self.pressed_keys.clone();
    self.pressed_keys = event_pump
      .keyboard_state()
      .pressed_scancodes()
      .filter_map(Keycode::from_scancode)
      .collect();
    self.new_keys = &self.pressed_keys - &self.prev_keys;
    self.released_keys = &self.prev_keys - &self.pressed_keys;
  }
}