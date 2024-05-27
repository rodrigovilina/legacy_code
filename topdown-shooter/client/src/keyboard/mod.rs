use {
  super::loggable::Loggable,
  sdl2::keyboard::{KeyboardState, Keycode},
  std::collections::HashSet,
};

pub struct Keyboard {
  pressed_keys: HashSet<Keycode>,
  held_keys: HashSet<Keycode>,
  released_keys: HashSet<Keycode>,
}

impl Keyboard {
  pub fn new() -> Self {
    let held_keys: HashSet<Keycode> = HashSet::new();
    let pressed_keys: HashSet<Keycode> = HashSet::new();
    let released_keys: HashSet<Keycode> = HashSet::new();

    Self {
      held_keys,
      pressed_keys,
      released_keys,
    }
  }

  pub fn update(&mut self, keyboard_state: KeyboardState) -> () {
    let keys: HashSet<Keycode> = keyboard_state
      .pressed_scancodes()
      .filter_map(Keycode::from_scancode)
      .collect();

    self.pressed_keys = &keys - &self.held_keys;
    self.released_keys = &self.held_keys - &keys;
    self.held_keys = keys;

    self.log().unwrap();
  }
}

impl Loggable for Keyboard {
  fn log_message(&self) -> String {
    format!(
      "\
Held Keys: {:?}
Pressed Keys: {:?}
Released Keys: {:?}\
",
      self.held_keys, self.pressed_keys, self.released_keys
    )
  }
}
