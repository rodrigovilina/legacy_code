use {
  super::loggable::Loggable,
  sdl2::{
    mouse::{MouseButton, MouseState, RelativeMouseState},
    EventPump,
  },
  std::collections::HashSet,
};

pub struct Mouse {
  pressed_buttons: HashSet<MouseButton>,
  held_buttons: HashSet<MouseButton>,
  released_buttons: HashSet<MouseButton>,
  mouse_state: MouseState,
  relative_mouse_state: RelativeMouseState,
  left_click_x: u128,
  left_click_y: u128,
}

impl Mouse {
  pub fn new(event_pump: &EventPump) -> Self {
    let pressed_buttons: HashSet<MouseButton> = HashSet::new();
    let held_buttons: HashSet<MouseButton> = HashSet::new();
    let released_buttons: HashSet<MouseButton> = HashSet::new();
    let mouse_state: MouseState = event_pump.mouse_state();
    let relative_mouse_state: RelativeMouseState = event_pump.relative_mouse_state();
    let left_click_x: u128 = 0;
    let left_click_y: u128 = 0;

    Self {
      pressed_buttons,
      held_buttons,
      released_buttons,
      mouse_state,
      relative_mouse_state,
      left_click_x,
      left_click_y,
    }
  }

  pub fn update(&mut self, mouse_state: MouseState) -> () {
    let buttons: HashSet<MouseButton> = mouse_state.pressed_mouse_buttons().collect();

    self.mouse_state = mouse_state;
    self.pressed_buttons = &buttons - &self.held_buttons;
    self.released_buttons = &self.held_buttons - &buttons;
    self.held_buttons = buttons;

    if self.pressed_buttons.contains(&MouseButton::Left) {
      self.left_click_x = self.mouse_state.x() as u128;
      self.left_click_y = self.mouse_state.y() as u128;
    }

    self.log().unwrap();
  }

  fn left_click(&self) -> (u128, u128) {
    (self.left_click_x, self.left_click_y)
  }
}

impl Loggable for Mouse {
  fn log_message(&self) -> String {
    format!(
      "\
{:?}
Held Buttons: {:?}
Pressed Buttons: {:?}
Released Buttons: {:?}
Left Click: {:?}\
",
      self.mouse_state,
      self.held_buttons,
      self.pressed_buttons,
      self.released_buttons,
      self.left_click()
    )
  }
}
