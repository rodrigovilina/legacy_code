use {
  sdl2::{mouse::MouseButton, EventPump},
  std::collections::HashSet,
};

#[derive(Debug)]
pub struct Mouse {
  pressed_buttons: HashSet<MouseButton>,
  prev_buttons: HashSet<MouseButton>,
  new_buttons: HashSet<MouseButton>,
  released_buttons: HashSet<MouseButton>,
  x: i32,
  y: i32,
}

impl Mouse {
  pub fn new() -> Self {
    Self {
      prev_buttons: HashSet::new(),
      pressed_buttons: HashSet::new(),
      new_buttons: HashSet::new(),
      released_buttons: HashSet::new(),
      x: 0,
      y: 0,
    }
  }

  pub fn x(&self) -> i32 {
    self.x
  }

  pub fn y(&self) -> i32 {
    self.y
  }

  pub fn released_left(&self) -> bool {
    self.released_buttons.contains(&MouseButton::Left)
  }

  pub fn update(&mut self, event_pump: &EventPump) {
    let mouse_state: &sdl2::mouse::MouseState = &event_pump.mouse_state();
    self.prev_buttons = self.pressed_buttons.clone();
    self.pressed_buttons = mouse_state.pressed_mouse_buttons().collect();
    self.new_buttons = &self.pressed_buttons - &self.prev_buttons;
    self.released_buttons = &self.prev_buttons - &self.pressed_buttons;
    self.x = mouse_state.x();
    self.y = mouse_state.y();

    // println!("{:?}", self);
  }
}
